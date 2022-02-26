#pragma once

class SequenceNumber {
public:
    SequenceNumber(uint32_t sequence_number) : m_sequence_number(sequence_number) {}

    uint32_t raw_sequence_number() const { return m_sequence_number; }

    bool operator==(const SequenceNumber& other) const { return this->m_sequence_number == other.m_sequence_number; }
    bool operator!=(const SequenceNumber& other) const { return this->m_sequence_number != other.m_sequence_number; }

    bool operator<(const SequenceNumber& other) const { return difference(other) < 0; }
    bool operator<=(const SequenceNumber& other) const { return difference(other) <= 0; }

    bool operator>(const SequenceNumber& other) const { return difference(other) > 0; }
    bool operator>=(const SequenceNumber& other) const { return difference(other) >= 0; }

    SequenceNumber& operator+=(uint32_t n) {
        this->m_sequence_number += n;
        this->m_sequence_number %= (MAX_SEQNO + 1);
        return *this;
    }
    SequenceNumber operator+(uint32_t n) const { return { (this->m_sequence_number + n) % (MAX_SEQNO + 1) }; }

    int difference(SequenceNumber n) const {
        int a = this->m_sequence_number;
        int b = n.m_sequence_number;

        // 3 Cases:
        // Case 1:
        // abs(a - b) <= MAX_SEQNO / 2
        //   -> return a - b
        //
        // Case 2:
        // abs(a - b) > MAX_SEQNO / 2 AND a < b
        //   -> return a + (MAX_SEQNO + 1) - b
        //
        // Case 3:
        // abs(a - b) > MAX_SEQNO / 2 AND a > b
        //   -> return -(b + MAX_SEQNO - a)
        //   -> return a - b - (MAX_SEQNO + 1)

        if (abs(a - b) <= MAX_SEQNO / 2) {
            return a - b;
        }
        if (a < b) {
            return a + (MAX_SEQNO + 1) - b;
        }
        return a - b - (MAX_SEQNO + 1);
    }

private:
    uint32_t m_sequence_number { 0 };
};

/*
static_assert(SequenceNumber { 1 } + 4 == SequenceNumber { 5 });
static_assert(SequenceNumber { 102400 } + 1 == SequenceNumber { 0 });
static_assert(SequenceNumber { 102400 } + 5 == SequenceNumber { 4 });
static_assert(SequenceNumber { 102398 } + 5 == SequenceNumber { 2 });
static_assert(SequenceNumber { 102398 } < 1);
static_assert(SequenceNumber { 1 } > 102398);
static_assert(SequenceNumber { 2 }.difference(102398) == 5);
static_assert(SequenceNumber { 102398 }.difference(2) == -5);
*/
