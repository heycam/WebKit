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

#pragma once

#include "ImageBuffer.h"
#include "Pattern.h"
#include "PatternAttributes.h"
#include "RenderSVGResourceContainer.h"
#include "SVGPatternElement.h"
#include <memory>
#include <wtf/IsoMallocInlines.h>
#include <wtf/HashMap.h>

namespace WebCore {

struct PatternData {
    WTF_MAKE_STRUCT_FAST_ALLOCATED;

    struct Inputs {
        bool operator==(const Inputs& other)
        {
            return objectBoundingBox == other.objectBoundingBox && absoluteTransformScale == other.absoluteTransformScale && textPaintingScale == other.textPaintingScale;
        }

        FloatRect objectBoundingBox;
        FloatSize absoluteTransformScale;
        float textPaintingScale = 1;
    };

    bool validate(const Inputs& inputs)
    {
        if (this->inputs != inputs) {
            pattern = nullptr;
            this->inputs = inputs;
        }
        return pattern;
    }

    RefPtr<Pattern> pattern;
    AffineTransform transform;
    Inputs inputs;
};

class RenderSVGResourcePattern final : public RenderSVGResourceContainer {
    WTF_MAKE_ISO_ALLOCATED(RenderSVGResourcePattern);
public:
    RenderSVGResourcePattern(SVGPatternElement&, RenderStyle&&);
    SVGPatternElement& patternElement() const;

    void removeAllClientsFromCache(bool markForInvalidation = true) override;
    void removeClientFromCache(RenderElement&, bool markForInvalidation = true) override;

    bool applyResource(RenderElement&, const RenderStyle&, GraphicsContext*&, OptionSet<RenderSVGResourceMode>) override;
    void postApplyResource(RenderElement&, GraphicsContext*&, OptionSet<RenderSVGResourceMode>, const Path*, const RenderElement*) override;
    FloatRect resourceBoundingBox(const RenderObject&) override { return FloatRect(); }

    RenderSVGResourceType resourceType() const override { return PatternResourceType; }

    void collectPatternAttributes(PatternAttributes&) const;

private:
    void element() const = delete;
    ASCIILiteral renderName() const override { return "RenderSVGResourcePattern"_s; }

    bool buildTileImageTransform(const FloatRect& objectBoundingBox, const PatternAttributes&, const SVGPatternElement&, FloatRect& patternBoundaries, AffineTransform& tileImageTransform) const;

    RefPtr<ImageBuffer> createTileImage(GraphicsContext&, const FloatSize&, const FloatSize& scale, const AffineTransform& tileImageTransform, const PatternAttributes&) const;

    PatternData::Inputs computeInputs(RenderElement&, OptionSet<RenderSVGResourceMode>);
    PatternData* buildPattern(RenderElement&, GraphicsContext&, const PatternData::Inputs&);
    bool updatePattern(PatternData&, GraphicsContext&);

    PatternAttributes m_attributes;
    HashMap<RenderElement*, std::unique_ptr<PatternData>> m_patternMap;
    bool m_shouldCollectPatternAttributes { true };
};

} // namespace WebCore

SPECIALIZE_TYPE_TRAITS_RENDER_SVG_RESOURCE(RenderSVGResourcePattern, PatternResourceType)
