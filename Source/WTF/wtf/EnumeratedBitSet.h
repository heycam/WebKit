/*
 * Copyright (C) 2022 Apple Inc. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY APPLE INC. ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL APPLE INC. OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#pragma once

#include <bitset>

namespace WTF {

// A wrapper around std::bitset that uses enum values in its API.
template <typename Key, Key lastValue>
class EnumeratedBitSet {
    WTF_MAKE_FAST_ALLOCATED;
private:
    using Bits = std::bitset<static_cast<size_t>(lastValue) + 1>;

public:
    constexpr EnumeratedBitSet() = default;

    constexpr bool operator==(const EnumeratedBitSet& other) const { return m_bits == other.m_bits; }
    constexpr bool operator!=(const EnumeratedBitSet& other) const { return m_bits != other.m_bits; }

    constexpr bool operator[](Key key) const { return m_bits[static_cast<size_t>(key)]; }
    typename Bits::reference operator[](Key key) { return m_bits[static_cast<size_t>(key)]; }

    bool test(Key key) const { return m_bits.test(static_cast<size_t>(key)); }

    bool all() const { return m_bits.all(); }
    bool any() const { return m_bits.any(); }
    bool none() const { return m_bits.none(); }
    size_t count() const { return m_bits.count(); }
    size_t size() const { return m_bits.size(); }

    EnumeratedBitSet& operator&=(const EnumeratedBitSet& other) { m_bits &= other.m_bits; return *this; }
    EnumeratedBitSet& operator|=(const EnumeratedBitSet& other) { m_bits |= other.m_bits; return *this; }
    EnumeratedBitSet& operator^=(const EnumeratedBitSet& other) { m_bits ^= other.m_bits; return *this; }
    constexpr EnumeratedBitSet operator~() const { return { ~m_bits }; };

    EnumeratedBitSet& set() { m_bits.set(); return *this; }
    EnumeratedBitSet& set(Key key, bool value = true) { m_bits.set(static_cast<size_t>(key), value); return *this; }
    EnumeratedBitSet& reset() { m_bits.reset(); return *this; }
    EnumeratedBitSet& reset(Key key) { m_bits.reset(static_cast<size_t>(key)); return *this; }
    EnumeratedBitSet& flip() { m_bits.flip(); return *this; }
    EnumeratedBitSet& flip(Key key) { m_bits.flip(static_cast<size_t>(key)); return *this; }

private:
    EnumeratedBitSet(const Bits& bits)
        : m_bits(bits)
    {
    }

    Bits m_bits;
};

} // namespace WTF

using WTF::EnumeratedBitSet;
