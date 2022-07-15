/*
 * Copyright (C) 2006 Nikolas Zimmermann <zimmermann@kde.org>
 * Copyright (C) Research In Motion Limited 2010. All rights reserved.
 * Copyright (C) 2022 Apple Inc. All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this library; see the file COPYING.LIB.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 */

#include "config.h"
#include "RenderSVGResourcePattern.h"

#include "ElementIterator.h"
#include "FrameView.h"
#include "GraphicsContext.h"
#include "LegacyRenderSVGRoot.h"
#include "SVGElementTypeHelpers.h"
#include "SVGFitToViewBox.h"
#include "SVGRenderingContext.h"
#include "SVGResources.h"
#include "SVGResourcesCache.h"
#include <wtf/IsoMallocInlines.h>

namespace WebCore {

WTF_MAKE_ISO_ALLOCATED_IMPL(RenderSVGResourcePattern);

RenderSVGResourcePattern::RenderSVGResourcePattern(SVGPatternElement& element, RenderStyle&& style)
    : RenderSVGResourceContainer(element, WTFMove(style))
{
}

SVGPatternElement& RenderSVGResourcePattern::patternElement() const
{
    return downcast<SVGPatternElement>(RenderSVGResourceContainer::element());
}

void RenderSVGResourcePattern::removeAllClientsFromCache(bool markForInvalidation)
{
    m_patternMap.clear();
    m_shouldCollectPatternAttributes = true;
    markAllClientsForInvalidation(markForInvalidation ? RepaintInvalidation : ParentOnlyInvalidation);
}

void RenderSVGResourcePattern::removeClientFromCache(RenderElement& client, bool markForInvalidation)
{
    m_patternMap.remove(&client);
    markClientForInvalidation(client, markForInvalidation ? RepaintInvalidation : ParentOnlyInvalidation);
}

void RenderSVGResourcePattern::collectPatternAttributes(PatternAttributes& attributes) const
{
    const RenderSVGResourcePattern* current = this;

    while (current) {
        const SVGPatternElement& pattern = current->patternElement();
        pattern.collectPatternAttributes(attributes);

        auto* resources = SVGResourcesCache::cachedResourcesForRenderer(*current);
        ASSERT_IMPLIES(resources && resources->linkedResource(), is<RenderSVGResourcePattern>(resources->linkedResource()));
        current = resources ? downcast<RenderSVGResourcePattern>(resources->linkedResource()) : nullptr;
    }
}

bool RenderSVGResourcePattern::updatePattern(PatternData& patternData, GraphicsContext& context)
{
    ASSERT(!m_shouldCollectPatternAttributes);

    // If we couldn't determine the pattern content element root, stop here.
    if (!m_attributes.patternContentElement())
        return false;

    // An empty viewBox disables rendering.
    if (m_attributes.hasViewBox() && m_attributes.viewBox().isEmpty())
        return false;

    // Compute all necessary transformations to build the tile image & the pattern.
    FloatRect tileBoundaries;
    AffineTransform tileImageTransform;
    if (!buildTileImageTransform(patternData.inputs.objectBoundingBox, m_attributes, patternElement(), tileBoundaries, tileImageTransform))
        return false;

    // Scale the tile size to match the scale level of the patternTransform.
    auto tileScale = patternData.inputs.absoluteTransformScale;
    tileScale.scale(static_cast<float>(m_attributes.patternTransform().xScale()), static_cast<float>(m_attributes.patternTransform().yScale()));

    // Build tile image.
    auto tileImage = createTileImage(context, tileBoundaries.size(), tileScale, tileImageTransform, m_attributes);
    if (!tileImage)
        return false;

    auto tileImageSize = tileImage->logicalSize();

    // Compute pattern space transformation.
    patternData.transform.translate(tileBoundaries.location());
    patternData.transform.scale(tileBoundaries.size() / tileImageSize);

    AffineTransform patternTransform = m_attributes.patternTransform();
    if (!patternTransform.isIdentity())
        patternData.transform = patternTransform * patternData.transform;

    // Account for text drawing resetting the context to non-scaled, see SVGInlineTextBox::paintTextWithShadows.
    if (patternData.inputs.textPaintingScale != 1)
        patternData.transform.scale(patternData.inputs.textPaintingScale);

    // Build pattern.
    patternData.pattern = Pattern::create({ tileImage.releaseNonNull() }, { true, true, patternData.transform });
    return true;
}

PatternData* RenderSVGResourcePattern::buildPattern(RenderElement& renderer, GraphicsContext& context, const PatternData::Inputs& inputs)
{
    // Various calls in updatePattern may trigger invalidations in some fringe
    // cases (ImageBuffer allocation failures in the SVG image cache for
    // example). To avoid having our PatternData deleted by removeAllClientsFromCache(),
    // we never call updatePattern while the PatternData is in m_patternMap.

    auto iter = m_patternMap.find(&renderer);

    if (iter == m_patternMap.end()) {
        auto patternData = makeUnique<PatternData>();
        patternData->inputs = inputs;
        if (!updatePattern(*patternData, context))
            return nullptr;
        return m_patternMap.set(&renderer, WTFMove(patternData)).iterator->value.get();
    }

    if (iter->value->validate(inputs))
        return iter->value.get();

    auto patternData = WTFMove(iter->value);
    if (!updatePattern(*patternData, context)) {
        m_patternMap.remove(&renderer);
        return nullptr;
    }
    return m_patternMap.set(&renderer, WTFMove(patternData)).iterator->value.get();
}

PatternData::Inputs RenderSVGResourcePattern::computeInputs(RenderElement& renderer, OptionSet<RenderSVGResourceMode> resourceMode)
{
    auto objectBoundingBox = renderer.objectBoundingBox();

    auto absoluteTransform = SVGRenderingContext::calculateTransformationToOutermostCoordinateSystem(renderer);
    FloatSize absoluteTransformScale(absoluteTransform.xScale(), absoluteTransform.yScale());

    float textPaintingScale = resourceMode.contains(RenderSVGResourceMode::ApplyToText) ? computeTextPaintingScale(renderer) : 1;

    return { objectBoundingBox, absoluteTransformScale, textPaintingScale };
}

bool RenderSVGResourcePattern::applyResource(RenderElement& renderer, const RenderStyle& style, GraphicsContext*& context, OptionSet<RenderSVGResourceMode> resourceMode)
{
    ASSERT(context);
    ASSERT(!resourceMode.isEmpty());

    if (m_shouldCollectPatternAttributes) {
        patternElement().synchronizeAllAttributes();

        m_attributes = PatternAttributes();
        collectPatternAttributes(m_attributes);
        m_shouldCollectPatternAttributes = false;
    }
    
    // Spec: When the geometry of the applicable element has no width or height and objectBoundingBox is specified,
    // then the given effect (e.g. a gradient or a filter) will be ignored.
    auto inputs = computeInputs(renderer, resourceMode);
    if (m_attributes.patternUnits() == SVGUnitTypes::SVG_UNIT_TYPE_OBJECTBOUNDINGBOX && inputs.objectBoundingBox.isEmpty())
        return false;

    auto patternData = buildPattern(renderer, *context, inputs);
    if (!patternData)
        return false;

    // Draw pattern
    context->save();

    const SVGRenderStyle& svgStyle = style.svgStyle();

    if (resourceMode.contains(RenderSVGResourceMode::ApplyToFill)) {
        context->setAlpha(svgStyle.fillOpacity());
        context->setFillPattern(*patternData->pattern);
        context->setFillRule(svgStyle.fillRule());
    } else if (resourceMode.contains(RenderSVGResourceMode::ApplyToStroke)) {
        if (svgStyle.vectorEffect() == VectorEffect::NonScalingStroke)
            patternData->pattern->setPatternSpaceTransform(transformOnNonScalingStroke(&renderer, patternData->transform));
        context->setAlpha(svgStyle.strokeOpacity());
        context->setStrokePattern(*patternData->pattern);
        SVGRenderSupport::applyStrokeStyleToContext(*context, style, renderer);
    }

    if (resourceMode.contains(RenderSVGResourceMode::ApplyToText)) {
        if (resourceMode.contains(RenderSVGResourceMode::ApplyToFill)) {
            context->setTextDrawingMode(TextDrawingMode::Fill);

#if USE(CG)
            context->applyFillPattern();
#endif
        } else if (resourceMode.contains(RenderSVGResourceMode::ApplyToStroke)) {
            context->setTextDrawingMode(TextDrawingMode::Stroke);

#if USE(CG)
            context->applyStrokePattern();
#endif
        }
    }

    return true;
}

void RenderSVGResourcePattern::postApplyResource(RenderElement&, GraphicsContext*& context, OptionSet<RenderSVGResourceMode> resourceMode, const Path* path, const RenderElement* shape)
{
    ASSERT(context);
    ASSERT(!resourceMode.isEmpty());
    fillAndStrokePathOrShape(*context, resourceMode, path, shape);
    context->restore();
}

static inline FloatRect calculatePatternBoundaries(const PatternAttributes& attributes,
                                                   const FloatRect& objectBoundingBox,
                                                   const SVGPatternElement& patternElement)
{
    return SVGLengthContext::resolveRectangle(&patternElement, attributes.patternUnits(), objectBoundingBox, attributes.x(), attributes.y(), attributes.width(), attributes.height());
}

bool RenderSVGResourcePattern::buildTileImageTransform(const FloatRect& objectBoundingBox,
                                                       const PatternAttributes& attributes,
                                                       const SVGPatternElement& patternElement,
                                                       FloatRect& patternBoundaries,
                                                       AffineTransform& tileImageTransform) const
{
    patternBoundaries = calculatePatternBoundaries(attributes, objectBoundingBox, patternElement); 
    if (patternBoundaries.width() <= 0 || patternBoundaries.height() <= 0)
        return false;

    AffineTransform viewBoxCTM = SVGFitToViewBox::viewBoxToViewTransform(attributes.viewBox(), attributes.preserveAspectRatio(), patternBoundaries.width(), patternBoundaries.height());

    // Apply viewBox/objectBoundingBox transformations.
    if (!viewBoxCTM.isIdentity())
        tileImageTransform = viewBoxCTM;
    else if (attributes.patternContentUnits() == SVGUnitTypes::SVG_UNIT_TYPE_OBJECTBOUNDINGBOX)
        tileImageTransform.scale(objectBoundingBox.width(), objectBoundingBox.height());

    return true;
}

RefPtr<ImageBuffer> RenderSVGResourcePattern::createTileImage(GraphicsContext& context, const FloatSize& size, const FloatSize& scale, const AffineTransform& tileImageTransform, const PatternAttributes& attributes) const
{
    // This is equivalent to making createImageBuffer() use roundedIntSize().
    auto roundedUnscaledImageBufferSize = [](const FloatSize& size, const FloatSize& scale) -> FloatSize {
        auto scaledSize = size * scale;
        return size - (expandedIntSize(scaledSize) - roundedIntSize(scaledSize)) * (scaledSize - flooredIntSize(scaledSize)) / scale;
    };

    auto tileSize = roundedUnscaledImageBufferSize(size, scale);

    // FIXME: Use createImageBuffer(rect, scale), delete the above calculations and fix 'tileImageTransform'
    auto tileImage = context.createScaledImageBuffer(tileSize, scale);
    if (!tileImage)
        return nullptr;

    GraphicsContext& tileImageContext = tileImage->context();

    // Apply tile image transformations.
    if (!tileImageTransform.isIdentity())
        tileImageContext.concatCTM(tileImageTransform);

    AffineTransform contentTransformation;
    if (attributes.patternContentUnits() == SVGUnitTypes::SVG_UNIT_TYPE_OBJECTBOUNDINGBOX)
        contentTransformation = tileImageTransform;

    // Draw the content into the ImageBuffer.
    for (auto& child : childrenOfType<SVGElement>(*attributes.patternContentElement())) {
        if (!child.renderer())
            continue;
        if (child.renderer()->needsLayout())
            return nullptr;
        SVGRenderingContext::renderSubtreeToContext(tileImageContext, *child.renderer(), contentTransformation);
    }

    return tileImage;
}

}
