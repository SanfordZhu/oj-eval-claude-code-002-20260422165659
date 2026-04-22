#include <complex>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <vector>

namespace sjtu {

constexpr int BASE = 1000000000;
constexpr int BASE_DIGITS = 9;

class BigInt {
public:
    std::vector<int> digits;
    bool negative;

    BigInt() : negative(false) {}

    BigInt(long long x) {
        if (x < 0) {
            negative = true;
            x = -x;
        } else {
            negative = false;
        }
        if (x == 0) {
            digits.push_back(0);
        } else {
            while (x > 0) {
                digits.push_back(x % BASE);
                x /= BASE;
            }
        }
    }

    BigInt(const std::string &s) {
        negative = false;
        size_t start = 0;
        if (s[0] == '-') {
            negative = true;
            start = 1;
        } else if (s[0] == '+') {
            start = 1;
        }
        size_t len = s.length() - start;
        for (size_t i = start; i < s.length(); i += BASE_DIGITS) {
            int end = std::min(s.length(), i + BASE_DIGITS);
            int val = 0;
            for (size_t j = i; j < end; j++) {
                val = val * 10 + (s[j] - '0');
            }
            digits.push_back(val);
        }
        while (digits.size() > 1 && digits.back() == 0) {
            digits.pop_back();
        }
        if (digits.empty() || (digits.size() == 1 && digits[0] == 0)) {
            negative = false;
        }
    }

    void removeLeadingZeros() {
        while (digits.size() > 1 && digits.back() == 0) {
            digits.pop_back();
        }
        if (digits.empty() || (digits.size() == 1 && digits[0] == 0)) {
            negative = false;
        }
    }

    bool isZero() const {
        return digits.size() == 1 && digits[0] == 0;
    }

    int absCompare(const BigInt &other) const {
        if (digits.size() != other.digits.size()) {
            return digits.size() < other.digits.size() ? -1 : 1;
        }
        for (int i = (int)digits.size() - 1; i >= 0; i--) {
            if (digits[i] != other.digits[i]) {
                return digits[i] < other.digits[i] ? -1 : 1;
            }
        }
        return 0;
    }

    bool operator<(const BigInt &other) const {
        if (negative != other.negative) {
            return negative;
        }
        if (negative) {
            return absCompare(other) > 0;
        }
        return absCompare(other) < 0;
    }

    bool operator==(const BigInt &other) const {
        if (negative != other.negative) return false;
        return absCompare(other) == 0;
    }

    BigInt addAbs(const BigInt &other) const {
        BigInt result;
        result.negative = false;
        result.digits.resize(std::max(digits.size(), other.digits.size()) + 1, 0);
        long long carry = 0;
        for (size_t i = 0; i < result.digits.size(); i++) {
            long long sum = carry;
            if (i < digits.size()) sum += digits[i];
            if (i < other.digits.size()) sum += other.digits[i];
            result.digits[i] = sum % BASE;
            carry = sum / BASE;
        }
        result.removeLeadingZeros();
        return result;
    }

    BigInt subAbs(const BigInt &other) const {
        BigInt result;
        result.negative = false;
        result.digits.resize(digits.size(), 0);
        long long borrow = 0;
        for (size_t i = 0; i < digits.size(); i++) {
            long long diff = (long long)digits[i] - borrow;
            if (i < other.digits.size()) diff -= other.digits[i];
            if (diff < 0) {
                diff += BASE;
                borrow = 1;
            } else {
                borrow = 0;
            }
            result.digits[i] = diff;
        }
        result.removeLeadingZeros();
        return result;
    }

    BigInt operator+(const BigInt &other) const {
        if (negative == other.negative) {
            BigInt result = addAbs(other);
            result.negative = negative;
            return result;
        }
        int cmp = absCompare(other);
        if (cmp == 0) {
            return BigInt((long long)0);
        }
        if (cmp > 0) {
            BigInt result = subAbs(other);
            result.negative = negative;
            return result;
        } else {
            BigInt result = other.subAbs(*this);
            result.negative = other.negative;
            return result;
        }
    }

    BigInt operator-() const {
        BigInt result = *this;
        if (!isZero()) {
            result.negative = !negative;
        }
        return result;
    }

    BigInt operator-(const BigInt &other) const {
        return *this + (-other);
    }

    BigInt mulSimple(const BigInt &other) const {
        BigInt result;
        result.negative = negative != other.negative;
        result.digits.resize(digits.size() + other.digits.size(), 0);
        for (size_t i = 0; i < digits.size(); i++) {
            long long carry = 0;
            for (size_t j = 0; j < other.digits.size() || carry > 0; j++) {
                long long prod = (long long)result.digits[i + j] + carry;
                if (j < other.digits.size()) {
                    prod += (long long)digits[i] * other.digits[j];
                }
                result.digits[i + j] = prod % BASE;
                carry = prod / BASE;
            }
        }
        result.removeLeadingZeros();
        return result;
    }

    BigInt operator*(const BigInt &other) const {
        return mulSimple(other);
    }

    BigInt divMod(const BigInt &other, bool getMod) const {
        if (other.isZero()) {
            return BigInt((long long)0);
        }

        int cmp = absCompare(other);
        if (cmp < 0) {
            if (getMod) {
                BigInt result = *this;
                result.negative = false;
                return result;
            }
            return BigInt((long long)0);
        }
        if (cmp == 0) {
            if (getMod) return BigInt((long long)0);
            BigInt result((long long)1);
            result.negative = negative != other.negative;
            return result;
        }

        BigInt dividend = *this;
        dividend.negative = false;
        BigInt divisor = other;
        divisor.negative = false;

        BigInt quotient, remainder;
        quotient.digits.resize(dividend.digits.size(), 0);

        for (int i = (int)dividend.digits.size() - 1; i >= 0; i--) {
            remainder.digits.insert(remainder.digits.begin(), dividend.digits[i]);
            remainder.removeLeadingZeros();

            int lo = 0, hi = BASE - 1;
            while (lo < hi) {
                int mid = (lo + hi + 1) / 2;
                BigInt temp = divisor;
                for (size_t j = 0; j < temp.digits.size(); j++) {
                    temp.digits[j] *= mid;
                }
                long long carry = 0;
                for (size_t j = 0; j < temp.digits.size(); j++) {
                    long long val = temp.digits[j] + carry;
                    temp.digits[j] = val % BASE;
                    carry = val / BASE;
                }
                if (carry > 0) temp.digits.push_back(carry);
                temp.removeLeadingZeros();

                if (remainder.absCompare(temp) >= 0) {
                    lo = mid;
                } else {
                    hi = mid - 1;
                }
            }

            quotient.digits[i] = lo;
            if (lo > 0) {
                BigInt temp = divisor;
                for (size_t j = 0; j < temp.digits.size(); j++) {
                    temp.digits[j] *= lo;
                }
                long long carry = 0;
                for (size_t j = 0; j < temp.digits.size(); j++) {
                    long long val = temp.digits[j] + carry;
                    temp.digits[j] = val % BASE;
                    carry = val / BASE;
                }
                if (carry > 0) temp.digits.push_back(carry);
                temp.removeLeadingZeros();
                remainder = remainder.subAbs(temp);
            }
        }

        quotient.removeLeadingZeros();
        remainder.removeLeadingZeros();

        if (getMod) {
            remainder.negative = negative;
            if (negative && !remainder.isZero()) {
                remainder = remainder + divisor;
            }
            return remainder;
        }

        quotient.negative = negative != other.negative;
        if (quotient.negative && !quotient.isZero()) {
            BigInt one((long long)1);
            quotient = quotient - one;
        }
        return quotient;
    }

    BigInt operator/(const BigInt &other) const {
        return divMod(other, false);
    }

    BigInt operator%(const BigInt &other) const {
        return divMod(other, true);
    }

    std::string toString() const {
        if (isZero()) return "0";
        std::string result;
        if (negative) result += "-";
        result += std::to_string(digits.back());
        for (int i = (int)digits.size() - 2; i >= 0; i--) {
            std::string part = std::to_string(digits[i]);
            while (part.length() < BASE_DIGITS) {
                part = "0" + part;
            }
            result += part;
        }
        return result;
    }
};

class int2048 {
  BigInt *data;
public:
  int2048();
  int2048(long long);
  int2048(const std::string &);
  int2048(const int2048 &);
  ~int2048();

  void read(const std::string &);
  void print();

  int2048 &add(const int2048 &);
  friend int2048 add(int2048, const int2048 &);

  int2048 &minus(const int2048 &);
  friend int2048 minus(int2048, const int2048 &);

  int2048 operator+() const;
  int2048 operator-() const;

  int2048 &operator=(const int2048 &);

  int2048 &operator+=(const int2048 &);
  friend int2048 operator+(int2048, const int2048 &);

  int2048 &operator-=(const int2048 &);
  friend int2048 operator-(int2048, const int2048 &);

  int2048 &operator*=(const int2048 &);
  friend int2048 operator*(int2048, const int2048 &);

  int2048 &operator/=(const int2048 &);
  friend int2048 operator/(int2048, const int2048 &);

  int2048 &operator%=(const int2048 &);
  friend int2048 operator%(int2048, const int2048 &);

  friend std::istream &operator>>(std::istream &, int2048 &);
  friend std::ostream &operator<<(std::ostream &, const int2048 &);

  friend bool operator==(const int2048 &, const int2048 &);
  friend bool operator!=(const int2048 &, const int2048 &);
  friend bool operator<(const int2048 &, const int2048 &);
  friend bool operator>(const int2048 &, const int2048 &);
  friend bool operator<=(const int2048 &, const int2048 &);
  friend bool operator>=(const int2048 &, const int2048 &);
};

int2048::int2048() : data(new BigInt()) {}

int2048::int2048(long long x) : data(new BigInt(x)) {}

int2048::int2048(const std::string &s) : data(new BigInt(s)) {}

int2048::int2048(const int2048 &other) : data(new BigInt(*other.data)) {}

int2048::~int2048() {
    delete data;
}

int2048 &int2048::operator=(const int2048 &other) {
    if (this != &other) {
        *data = *other.data;
    }
    return *this;
}

void int2048::read(const std::string &s) {
    *data = BigInt(s);
}

void int2048::print() {
    std::cout << data->toString();
}

int2048 &int2048::add(const int2048 &other) {
    *data = *data + *other.data;
    return *this;
}

int2048 add(int2048 a, const int2048 &b) {
    return a.add(b);
}

int2048 &int2048::minus(const int2048 &other) {
    *data = *data - *other.data;
    return *this;
}

int2048 minus(int2048 a, const int2048 &b) {
    return a.minus(b);
}

int2048 int2048::operator+() const {
    return *this;
}

int2048 int2048::operator-() const {
    int2048 result;
    *result.data = -*data;
    return result;
}

int2048 &int2048::operator+=(const int2048 &other) {
    return add(other);
}

int2048 operator+(int2048 a, const int2048 &b) {
    return a.add(b);
}

int2048 &int2048::operator-=(const int2048 &other) {
    return minus(other);
}

int2048 operator-(int2048 a, const int2048 &b) {
    return a.minus(b);
}

int2048 &int2048::operator*=(const int2048 &other) {
    *data = *data * *other.data;
    return *this;
}

int2048 operator*(int2048 a, const int2048 &b) {
    return a *= b;
}

int2048 &int2048::operator/=(const int2048 &other) {
    *data = *data / *other.data;
    return *this;
}

int2048 operator/(int2048 a, const int2048 &b) {
    return a /= b;
}

int2048 &int2048::operator%=(const int2048 &other) {
    *data = *data % *other.data;
    return *this;
}

int2048 operator%(int2048 a, const int2048 &b) {
    return a %= b;
}

std::istream &operator>>(std::istream &is, int2048 &x) {
    std::string s;
    is >> s;
    x.read(s);
    return is;
}

std::ostream &operator<<(std::ostream &os, const int2048 &x) {
    os << x.data->toString();
    return os;
}

bool operator==(const int2048 &a, const int2048 &b) {
    return *a.data == *b.data;
}

bool operator!=(const int2048 &a, const int2048 &b) {
    return !(a == b);
}

bool operator<(const int2048 &a, const int2048 &b) {
    return *a.data < *b.data;
}

bool operator>(const int2048 &a, const int2048 &b) {
    return b < a;
}

bool operator<=(const int2048 &a, const int2048 &b) {
    return !(b < a);
}

bool operator>=(const int2048 &a, const int2048 &b) {
    return !(a < b);
}

} // namespace sjtu
