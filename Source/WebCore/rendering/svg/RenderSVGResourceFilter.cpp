/*
 * Copyright (C) 2004, 2005, 2006, 2007 Nikolas Zimmermann <zimmermann@kde.org>
 * Copyright (C) 2004, 2005 Rob Buis <buis@kde.org>
 * Copyright (C) 2005 Eric Seidel <eric@webkit.org>
 * Copyright (C) 2009 Dirk Schulze <krit@webkit.org>
 * Copyright (C) Research In Motion Limited 2010. All rights reserved.
 * Copyright (C) 2021-2022 Apple Inc. All rights reserved.
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
#include "RenderSVGResourceFilter.h"

#include "FilterEffect.h"
#include "FloatPoint.h"
#include "GraphicsContext.h"
#include "IntRect.h"
#include "Logging.h"
#include "RenderSVGResourceFilterInlines.h"
#include "SVGElementTypeHelpers.h"
#include "SVGRenderingContext.h"
#include <wtf/IsoMallocInlines.h>
#include <wtf/text/TextStream.h>

namespace WebCore {

WTF_MAKE_ISO_ALLOCATED_IMPL(FilterData);
WTF_MAKE_ISO_ALLOCATED_IMPL(RenderSVGResourceFilter);

RenderSVGResourceFilter::RenderSVGResourceFilter(SVGFilterElement& element, RenderStyle&& style)
    : RenderSVGResourceContainer(element, WTFMove(style))
{
}

RenderSVGResourceFilter::~RenderSVGResourceFilter() = default;

void RenderSVGResourceFilter::removeAllClientsFromCache(bool markForInvalidation)
{
    LOG(Filters, "RenderSVGResourceFilter %p removeAllClientsFromCache", this);

    m_filterMap.clear();

    markAllClientsForInvalidation(markForInvalidation ? LayoutAndBoundariesInvalidation : ParentOnlyInvalidation);
}

void RenderSVGResourceFilter::removeClientFromCache(RenderElement& client, bool markForInvalidation)
{
    LOG(Filters, "RenderSVGResourceFilter %p removing client %p", this, &client);
    
    auto findResult = m_filterMap.find(&client);
    if (findResult != m_filterMap.end()) {
        FilterData& filterData = *findResult->value;
        if (filterData.savedContext)
            filterData.state = FilterData::MarkedForRemoval;
        else
            m_filterMap.remove(findResult);
    }

    markClientForInvalidation(client, markForInvalidation ? BoundariesInvalidation : ParentOnlyInvalidation);
}

FilterData::Inputs RenderSVGResourceFilter::computeInputs(RenderElement& renderer)
{
    auto objectBoundingBox = renderer.objectBoundingBox();
    auto strokeBoundingBox = renderer.strokeBoundingBox();
    FloatSize absoluteTransformScale;

    // Determine absolute transformation matrix for filter. 
    auto absoluteTransform = SVGRenderingContext::calculateTransformationToOutermostCoordinateSystem(renderer);

    // Eliminate shear of the absolute transformation matrix, to be able to produce unsheared tile images for feTile.
    if (absoluteTransform.isInvertible())
        absoluteTransformScale = FloatSize(absoluteTransform.xScale(), absoluteTransform.yScale());

    return { objectBoundingBox, strokeBoundingBox, absoluteTransformScale };
}

bool RenderSVGResourceFilter::applyResource(RenderElement& renderer, const RenderStyle&, GraphicsContext*& context, OptionSet<RenderSVGResourceMode> resourceMode)
{
    ASSERT(context);
    ASSERT_UNUSED(resourceMode, !resourceMode);

    LOG(Filters, "RenderSVGResourceFilter %p applyResource renderer %p", this, &renderer);

    auto& filterData = *m_filterMap.ensure(&renderer, [&] {
        return makeUnique<FilterData>();
    }).iterator->value;

    switch (filterData.state) {
    case FilterData::Empty:
    case FilterData::Built:
        break;
    case FilterData::PaintingSource:
    case FilterData::Applying:
        filterData.state = FilterData::CycleDetected;
        return false;
    case FilterData::CycleDetected:
    case FilterData::MarkedForRemoval:
        return false;
    }
 
    if (!filterData.validate(computeInputs(renderer))) {
        if (filterData.inputs.absoluteTransformScale.isZero())
            return false;

        auto filterRegion = SVGLengthContext::resolveRectangle<SVGFilterElement>(&filterElement(), filterElement().filterUnits(), filterData.inputs.objectBoundingBox);
        if (filterRegion.isEmpty())
            return false;

        // Determine absolute boundaries of the filter and the drawing region.
        filterData.sourceImageRect = filterData.inputs.strokeBoundingBox;
        filterData.sourceImageRect.intersect(filterRegion);

        // Determine scale factor for filter. The size of intermediate ImageBuffers shouldn't be bigger than kMaxFilterSize.
        filterData.filterScale = filterData.inputs.absoluteTransformScale;
        ImageBuffer::sizeNeedsClamping(filterData.sourceImageRect.size(), filterData.filterScale);

        // Set the rendering mode from the page's settings.
        auto renderingMode = renderer.page().acceleratedFiltersEnabled() ? RenderingMode::Accelerated : RenderingMode::Unaccelerated;

        // Create the SVGFilter object.
        filterData.filter = SVGFilter::create(filterElement(), renderingMode, filterData.filterScale, Filter::ClipOperation::Intersect, filterRegion, filterData.inputs.objectBoundingBox, *context);
        if (!filterData.filter)
            return false;

        if (filterData.filter->clampFilterRegionIfNeeded())
            filterData.filterScale = filterData.filter->filterScale();
    }

    // If the sourceImageRect is empty, we have something like <g filter=".."/>.
    // Even if the target objectBoundingBox() is empty, we still have to draw the last effect result image in postApplyResource.
    if (filterData.sourceImageRect.isEmpty()) {
        ASSERT(m_filterMap.contains(&renderer));
        filterData.savedContext = context;
        return false;
    }

#if ENABLE(DESTINATION_COLOR_SPACE_LINEAR_SRGB)
    auto colorSpace = DestinationColorSpace::LinearSRGB();
#else
    auto colorSpace = DestinationColorSpace::SRGB();
#endif

    filterData.sourceImage = context->createScaledImageBuffer(filterData.sourceImageRect, filterData.filterScale, colorSpace, filterData.filter->renderingMode());
    if (!filterData.sourceImage) {
        ASSERT(m_filterMap.contains(&renderer));
        filterData.savedContext = context;
        return false;
    }

    filterData.savedContext = context;
    context = &filterData.sourceImage->context();

    ASSERT(m_filterMap.contains(&renderer));
    return true;
}

void RenderSVGResourceFilter::postApplyResource(RenderElement& renderer, GraphicsContext*& context, OptionSet<RenderSVGResourceMode> resourceMode, const Path*, const RenderElement*)
{
    ASSERT(context);
    ASSERT_UNUSED(resourceMode, !resourceMode);

    auto findResult = m_filterMap.find(&renderer);
    if (findResult == m_filterMap.end())
        return;

    FilterData& filterData = *findResult->value;

    LOG_WITH_STREAM(Filters, stream << "\nRenderSVGResourceFilter " << this << " postApplyResource - renderer " << &renderer << " filter state " << filterData.state);

    switch (filterData.state) {
    case FilterData::MarkedForRemoval:
        m_filterMap.remove(findResult);
        return;

    case FilterData::CycleDetected:
    case FilterData::Applying:
        // We have a cycle if we are already applying the data.
        // This can occur due to FeImage referencing a source that makes use of the FEImage itself.
        // This is the first place we've hit the cycle, so set the state back to PaintingSource so the return stack
        // will continue correctly.
        filterData.state = FilterData::PaintingSource;
        return;

    case FilterData::PaintingSource:
        if (!filterData.savedContext) {
            removeClientFromCache(renderer);
            return;
        }

        context = filterData.savedContext;
        filterData.savedContext = nullptr;
        break;

    case FilterData::Empty:
    case FilterData::Built:
        break;
    }

    if (filterData.filter) {
        filterData.state = FilterData::Built;
        context->drawFilteredImageBuffer(filterData.sourceImage.get(), filterData.sourceImageRect, *filterData.filter, filterData.results);
    }

    LOG_WITH_STREAM(Filters, stream << "RenderSVGResourceFilter " << this << " postApplyResource done\n");
}

FloatRect RenderSVGResourceFilter::resourceBoundingBox(const RenderObject& object)
{
    return SVGLengthContext::resolveRectangle<SVGFilterElement>(&filterElement(), filterElement().filterUnits(), object.objectBoundingBox());
}

void RenderSVGResourceFilter::markFilterForRepaint(FilterEffect& effect)
{
    LOG(Filters, "RenderSVGResourceFilter %p markFilterForRepaint effect %p", this, &effect);

    for (const auto& objectFilterDataPair : m_filterMap) {
        const auto& filterData = objectFilterDataPair.value;
        if (filterData->state != FilterData::Built)
            continue;

        // Repaint the image on the screen.
        markClientForInvalidation(*objectFilterDataPair.key, RepaintInvalidation);

        filterData->results.clearEffectResult(effect);
    }
}

void RenderSVGResourceFilter::markFilterForRebuild()
{
    LOG(Filters, "RenderSVGResourceFilter %p markFilterForRebuild", this);

    removeAllClientsFromCache();
}

FloatRect RenderSVGResourceFilter::drawingRegion(RenderObject* object) const
{
    FilterData* filterData = m_filterMap.get(object);
    return filterData ? filterData->sourceImageRect : FloatRect();
}

TextStream& operator<<(TextStream& ts, FilterData::State state)
{
    switch (state) {
    case FilterData::Empty:
        ts << "empty";
        break;
    case FilterData::PaintingSource:
        ts << "painting source";
        break;
    case FilterData::Applying:
        ts << "applying";
        break;
    case FilterData::Built:
        ts << "built";
        break;
    case FilterData::CycleDetected:
        ts << "cycle detected";
        break;
    case FilterData::MarkedForRemoval:
        ts << "marked for removal";
        break;
    }
    return ts;
}

} // namespace WebCore
