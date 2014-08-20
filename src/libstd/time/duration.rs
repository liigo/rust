// Copyright 2012-2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Temporal quantification

#![experimental]

use {fmt, i64};
use ops::{Add, Sub, Mul, Div, Neg};
use option::{Option, Some, None};
use num::{mod, CheckedAdd, CheckedSub, CheckedMul};
use result::{Result, Ok, Err};


/// `Duration`'s `secs` component should have no more than this value.
static MIN_SECS: i64 = i64::MIN;
/// `Duration`'s `secs` component should have no less than this value.
static MAX_SECS: i64 = i64::MAX;

/// The number of nanoseconds in seconds.
static NANOS_PER_SEC: i32 = 1_000_000_000;
/// The number of (non-leap) seconds in hours.
static SECS_PER_HOUR: i32 = 3600;
/// The number of (non-leap) seconds in days.
static SECS_PER_DAY: i32 = SECS_PER_HOUR * 24;
/// The number of (non-leap) seconds in weeks.
static SECS_PER_WEEK: i32 = SECS_PER_DAY * 7;

macro_rules! try_opt(
    ($e:expr) => (match $e { Some(v) => v, None => return None })
)


/// ISO 8601 time duration with nanosecond precision.
/// This also allows for the negative duration; see individual methods for details.
#[deriving(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Duration {
    secs: i64,
    nanos: u32, // Always < NANOS_PR_SECOND
}

/// The minimum possible `Duration`.
pub static MIN: Duration = Duration { secs: MIN_SECS, nanos: 0 };
/// The maximum possible `Duration`.
pub static MAX: Duration = Duration { secs: MAX_SECS, nanos: NANOS_PER_SEC as u32 - 1 };

impl Duration {
    /// Makes a new `Duration` with given number of weeks.
    /// Equivalent to `Duration::new(weeks * 7 * 24 * 60 * 60, 0)` with overflow checks.
    #[inline]
    pub fn weeks(weeks: i32) -> Duration {
        Duration { secs: (weeks as i64) * (SECS_PER_WEEK as i64), nanos: 0 }
    }

    /// Makes a new `Duration` with given number of days.
    /// Equivalent to `Duration::new(days * 24 * 60 * 60, 0)`.
    #[inline]
    pub fn days(days: i32) -> Duration {
        Duration { secs: (days as i64) * (SECS_PER_DAY as i64), nanos: 0 }
    }

    /// Makes a new `Duration` with given number of hours.
    /// Equivalent to `Duration::new(hours * 60 * 60, 0)` with overflow checks.
    #[inline]
    pub fn hours(hours: i32) -> Duration {
        Duration { secs: hours as i64 * 3600, nanos: 0 }
    }

    /// Makes a new `Duration` with given number of minutes.
    /// Equivalent to `Duration::new(mins * 60, 0)` with overflow checks.
    #[inline]
    pub fn minutes(mins: i32) -> Duration {
        Duration { secs: mins as i64 * 60, nanos: 0 }
    }

    /// Makes a new `Duration` with given number of seconds.
    /// Equivalent to `Duration::new(secs, 0)`.
    #[inline]
    pub fn seconds(secs: i64) -> Duration {
        Duration { secs: secs, nanos: 0 }
    }

    /// Makes a new `Duration` with given number of milliseconds.
    /// Equivalent to `Duration::new(0, millis * 1_000_000)` with overflow checks.
    #[inline]
    pub fn milliseconds(millis: i32) -> Duration {
        let (secs, millis) = div_mod_floor(millis, 1000);
        let nanos = millis * 1_000_000;
        Duration { secs: secs as i64, nanos: nanos as u32 }
    }

    /// Makes a new `Duration` with given number of microseconds.
    /// Equivalent to `Duration::new(0, micros * 1_000)` with overflow checks.
    #[inline]
    pub fn microseconds(micros: i32) -> Duration {
        let (secs, micros) = div_mod_floor(micros, 1_000_000);
        let nanos = micros * 1_000;
        Duration { secs: secs as i64, nanos: nanos as u32 }
    }

    /// Makes a new `Duration` with given number of nanoseconds.
    /// Equivalent to `Duration::new(0, nanos)`.
    #[inline]
    pub fn nanoseconds(nanos: i32) -> Duration {
        let (secs, nanos) = div_mod_floor(nanos, NANOS_PER_SEC);
        Duration { secs: secs as i64, nanos: nanos as u32 }
    }

    /// Returns a tuple of the number of (non-leap) seconds and
    /// nanoseconds in the duration.  Note that the number of
    /// nanoseconds are always positive, so that for example
    /// `-Duration::nanoseconds(3)` has -1 seconds and 999_999_997 nanoseconds.
    #[inline]
    fn to_tuple_64(&self) -> (i64, u32) {
        (self.secs, self.nanos)
    }

    /// Negates the duration and returns a tuple like `to_tuple`.
    /// This does not overflow and thus is internally used for several methods.
    fn to_negated_tuple_64(&self) -> (i64, u32) {
        let mut secs = -(self.secs as i64);
        let mut nanos = -(self.nanos as i32);
        if nanos < 0 {
            nanos += NANOS_PER_SEC;
            secs -= 1;
        }
        (secs as i64, nanos as u32)
    }

    /// Returns the total number of whole weeks in the duration.
    #[inline]
    pub fn num_weeks(&self) -> i32 {
        self.num_days() / 7
    }

    /// Returns the total number of whole days in the duration.
    pub fn num_days(&self) -> i32 {
        if self.secs < 0 {
            let negated = -*self;
            - (negated.secs / (SECS_PER_DAY as i64)) as i32
        } else {
            (self.secs / (SECS_PER_DAY as i64)) as i32
        }
    }

    /// Returns the total number of whole hours in the duration.
    #[inline]
    pub fn num_hours(&self) -> i64 {
        self.num_seconds() / 3600
    }

    /// Returns the total number of whole minutes in the duration.
    #[inline]
    pub fn num_minutes(&self) -> i64 {
        self.num_seconds() / 60
    }

    /// Returns the total number of whole seconds in the duration.
    pub fn num_seconds(&self) -> i64 {
        // cannot overflow, 2^32 * 86400 < 2^64
        fn secs((secs, _): (i64, u32)) -> i64 {
            secs
        }
        if self.secs < 0 {-secs(self.to_negated_tuple_64())} else {secs(self.to_tuple_64())}
    }

    /// Returns the total number of whole milliseconds in the duration.
    pub fn num_milliseconds(&self) -> Option<i64> {
        fn millis((secs, nanos): (i64, u32)) -> Option<i64> {
            static MILLIS_PER_SEC: i64 = 1_000;
            static NANOS_PER_MILLI: i64 = 1_000_000;
            let nmillis = try_opt!(secs.checked_mul(&(MILLIS_PER_SEC as i64)));
            let nmillis = try_opt!(nmillis.checked_add(&(nanos as i64 / NANOS_PER_MILLI)));
            Some(nmillis)
        }
        if self.secs < 0 {
            millis(self.to_negated_tuple_64()).map(|millis| -millis)
        } else {
            millis(self.to_tuple_64())
        }
    }

    /// Returns the total number of whole microseconds in the duration,
    /// or `None` on the overflow (exceeding 2^63 microseconds in either directions).
    pub fn num_microseconds(&self) -> Option<i64> {
        fn micros((secs, nanos): (i64, u32)) -> Option<i64> {
            static MICROS_PER_SEC: i64 = 1_000_000;
            static NANOS_PER_MICRO: i64 = 1_000;
            let nmicros = try_opt!(secs.checked_mul(&(MICROS_PER_SEC as i64)));
            let nmicros = try_opt!(nmicros.checked_add(&(nanos as i64 / NANOS_PER_MICRO as i64)));
            Some(nmicros)
        }
        if self.secs < 0 {
            // the final negation won't overflow since we start with positive numbers.
            micros(self.to_negated_tuple_64()).map(|micros| -micros)
        } else {
            micros(self.to_tuple_64())
        }
    }

    /// Returns the total number of whole nanoseconds in the duration,
    /// or `None` on the overflow (exceeding 2^63 nanoseconds in either directions).
    pub fn num_nanoseconds(&self) -> Option<i64> {
        fn nanos((secs, nanos): (i64, u32)) -> Option<i64> {
            let nnanos = try_opt!(secs.checked_mul(&(NANOS_PER_SEC as i64)));
            let nnanos = try_opt!(nnanos.checked_add(&(nanos as i64)));
            Some(nnanos)
        }
        if self.secs < 0 {
            // the final negation won't overflow since we start with positive numbers.
            nanos(self.to_negated_tuple_64()).map(|nanos| -nanos)
        } else {
            nanos(self.to_tuple_64())
        }
    }
}

impl num::Bounded for Duration {
    #[inline] fn min_value() -> Duration { MIN }
    #[inline] fn max_value() -> Duration { MAX }
}

impl num::Zero for Duration {
    #[inline]
    fn zero() -> Duration {
        Duration { secs: 0, nanos: 0 }
    }

    #[inline]
    fn is_zero(&self) -> bool {
        self.secs == 0 && self.nanos == 0
    }
}

impl Neg<Duration> for Duration {
    #[inline]
    fn neg(&self) -> Duration {
        let (secs, nanos) = self.to_negated_tuple_64();
        Duration { secs: secs, nanos: nanos } // FIXME can overflow
    }
}

impl Add<Duration,Duration> for Duration {
    fn add(&self, rhs: &Duration) -> Duration {
        let mut secs = self.secs + rhs.secs;
        let mut nanos = self.nanos + rhs.nanos;
        if nanos >= NANOS_PER_SEC as u32 {
            nanos -= NANOS_PER_SEC as u32;
            secs += 1;
        }
        Duration { secs: secs, nanos: nanos }
    }
}

impl num::CheckedAdd for Duration {
    fn checked_add(&self, rhs: &Duration) -> Option<Duration> {
        let mut secs = try_opt!(self.secs.checked_add(&rhs.secs));
        let mut nanos = self.nanos + rhs.nanos;
        if nanos >= NANOS_PER_SEC as u32 {
            nanos -= NANOS_PER_SEC as u32;
            secs += 1;
        }
        Some(Duration { secs: secs, nanos: nanos })
    }
}

impl Sub<Duration,Duration> for Duration {
    fn sub(&self, rhs: &Duration) -> Duration {
        let mut secs = self.secs - rhs.secs;
        let mut nanos = self.nanos as i32 - rhs.nanos as i32;
        if nanos < 0 {
            nanos += NANOS_PER_SEC;
            secs -= 1;
        }
        Duration { secs: secs, nanos: nanos as u32 }
    }
}

impl num::CheckedSub for Duration {
    fn checked_sub(&self, rhs: &Duration) -> Option<Duration> {
        let mut secs = try_opt!(self.secs.checked_sub(&rhs.secs));
        let mut nanos = self.nanos as i32 - rhs.nanos as i32;
        if nanos < 0 {
            nanos += NANOS_PER_SEC;
            secs -= 1;
        }
        Some(Duration { secs: secs, nanos: nanos as u32 })
    }
}

impl Mul<i32,Duration> for Duration {
    fn mul(&self, rhs: &i32) -> Duration {
        /// Given `0 <= y < limit <= 2^30`,
        /// returns `(h,l)` such that `x * y = h * limit + l` where `0 <= l < limit`.
        fn mul_i64_u32_limit(x: i64, y: u32, limit: u32) -> (i64,u32) {
            let y = y as i64;
            let limit = limit as i64;
            let (xh, xl) = div_mod_floor_64(x, limit);
            let (h, l) = (xh * y, xl * y);
            let (h_, l) = div_rem_64(l, limit);
            (h + h_, l as u32)
        }

        let rhs = *rhs as i64;
        let (secs1, nanos) = mul_i64_u32_limit(rhs, self.nanos, NANOS_PER_SEC as u32);
        let secs2 = rhs * self.secs;
        Duration { secs: secs1 + secs2, nanos: nanos }
    }
}

impl Div<i32,Duration> for Duration {
    fn div(&self, rhs: &i32) -> Duration {
        let (rhs, secs, nanos) = if *rhs < 0 {
            let (secs, nanos) = self.to_negated_tuple_64();
            (-(*rhs as i64), secs as i64, nanos as i64)
        } else {
            (*rhs as i64, self.secs as i64, self.nanos as i64)
        };

        let (secs, carry) = div_mod_floor_64(secs, rhs);
        let nanos = nanos + carry * NANOS_PER_SEC as i64;
        let nanos = nanos / rhs;
        Duration { secs: secs as i64, nanos: nanos as u32 }
    }
}

impl fmt::Show for Duration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let hasdate = self.num_days() != 0;
        let hastime = (self.secs % (SECS_PER_DAY as i64) != 0 || self.nanos != 0) || !hasdate;

        try!(write!(f, "P"));
        if hasdate {
            // technically speaking the negative part is not the valid ISO 8601,
            // but we need to print it anyway.
            try!(write!(f, "{}D", self.num_days()));
        }
        if hastime {
            let secs = self.secs % (SECS_PER_DAY as i64);
            if self.nanos == 0 {
                try!(write!(f, "T{}S", secs));
            } else if self.nanos % 1_000_000 == 0 {
                try!(write!(f, "T{}.{:03}S", secs, self.nanos / 1_000_000));
            } else if self.nanos % 1_000 == 0 {
                try!(write!(f, "T{}.{:06}S", secs, self.nanos / 1_000));
            } else {
                try!(write!(f, "T{}.{:09}S", secs, self.nanos));
            }
        }
        Ok(())
    }
}

// Copied from libnum
#[inline]
fn div_mod_floor(this: i32, other: i32) -> (i32, i32) {
    (div_floor(this, other), mod_floor(this, other))
}

#[inline]
fn div_floor(this: i32, other: i32) -> i32 {
    match div_rem(this, other) {
        (d, r) if (r > 0 && other < 0)
               || (r < 0 && other > 0) => d - 1,
        (d, _)                         => d,
    }
}

#[inline]
fn mod_floor(this: i32, other: i32) -> i32 {
    match this % other {
        r if (r > 0 && other < 0)
          || (r < 0 && other > 0) => r + other,
        r                         => r,
    }
}

#[inline]
fn div_rem(this: i32, other: i32) -> (i32, i32) {
    (this / other, this % other)
}

#[inline]
fn div_mod_floor_64(this: i64, other: i64) -> (i64, i64) {
    (div_floor_64(this, other), mod_floor_64(this, other))
}

#[inline]
fn div_floor_64(this: i64, other: i64) -> i64 {
    match div_rem_64(this, other) {
        (d, r) if (r > 0 && other < 0)
               || (r < 0 && other > 0) => d - 1,
        (d, _)                         => d,
    }
}

#[inline]
fn mod_floor_64(this: i64, other: i64) -> i64 {
    match this % other {
        r if (r > 0 && other < 0)
          || (r < 0 && other > 0) => r + other,
        r                         => r,
    }
}

#[inline]
fn div_rem_64(this: i64, other: i64) -> (i64, i64) {
    (this / other, this % other)
}

#[cfg(test)]
mod tests {
    use super::{Duration, MIN, MAX, MAX_SECS, MIN_SECS};
    use {i32, i64};
    use num::{Zero, CheckedAdd, CheckedSub};
    use option::{Some, None};
    use to_string::ToString;

    #[test]
    fn test_duration() {
        let d: Duration = Zero::zero();
        assert_eq!(d, Zero::zero());
        assert!(Duration::seconds(1) != Zero::zero());
        assert_eq!(Duration::seconds(1) + Duration::seconds(2), Duration::seconds(3));
        assert_eq!(Duration::seconds(86399) + Duration::seconds(4),
                   Duration::days(1) + Duration::seconds(3));
        assert_eq!(Duration::days(10) - Duration::seconds(1000), Duration::seconds(863000));
        assert_eq!(Duration::days(10) - Duration::seconds(1000000), Duration::seconds(-136000));
        assert_eq!(Duration::days(2) + Duration::seconds(86399) + Duration::nanoseconds(1234567890),
                   Duration::days(3) + Duration::nanoseconds(234567890));
        assert_eq!(-Duration::days(3), Duration::days(-3));
        assert_eq!(-(Duration::days(3) + Duration::seconds(70)),
                   Duration::days(-4) + Duration::seconds(86400-70));
    }

    #[test]
    fn test_duration_num_days() {
        let d: Duration = Zero::zero();
        assert_eq!(d.num_days(), 0);
        assert_eq!(Duration::days(1).num_days(), 1);
        assert_eq!(Duration::days(-1).num_days(), -1);
        assert_eq!(Duration::seconds(86399).num_days(), 0);
        assert_eq!(Duration::seconds(86401).num_days(), 1);
        assert_eq!(Duration::seconds(-86399).num_days(), 0);
        assert_eq!(Duration::seconds(-86401).num_days(), -1);
        assert_eq!(Duration::seconds(i64::MAX).num_seconds(), i64::MAX);
        assert_eq!(Duration::seconds(i64::MIN).num_seconds(), i64::MIN);
        assert_eq!(MAX.num_seconds(), MAX_SECS);
        assert_eq!(MIN.num_seconds(), MIN_SECS);
    }

    #[test]
    fn test_duration_num_seconds() {
        let d: Duration = Zero::zero();
        assert_eq!(d.num_seconds(), 0);
        assert_eq!(Duration::seconds(1).num_seconds(), 1);
        assert_eq!(Duration::seconds(-1).num_seconds(), -1);
        assert_eq!(Duration::milliseconds(999).num_seconds(), 0);
        assert_eq!(Duration::milliseconds(1001).num_seconds(), 1);
        assert_eq!(Duration::milliseconds(-999).num_seconds(), 0);
        assert_eq!(Duration::milliseconds(-1001).num_seconds(), -1);
        assert_eq!(Duration::milliseconds(i32::MAX).num_milliseconds(), Some(i32::MAX as i64));
        assert_eq!(Duration::milliseconds(i32::MIN).num_milliseconds(), Some(i32::MIN as i64));
        assert_eq!(MAX.num_milliseconds(), None);
        assert_eq!(MIN.num_milliseconds(), None);
    }

    #[test]
    fn test_duration_num_milliseconds() {
        let d: Duration = Zero::zero();
        assert_eq!(d.num_milliseconds(), Some(0));
        assert_eq!(Duration::milliseconds(1).num_milliseconds(), Some(1));
        assert_eq!(Duration::milliseconds(-1).num_milliseconds(), Some(-1));
        assert_eq!(Duration::microseconds(999).num_milliseconds(), Some(0));
        assert_eq!(Duration::microseconds(1001).num_milliseconds(), Some(1));
        assert_eq!(Duration::microseconds(-999).num_milliseconds(), Some(0));
        assert_eq!(Duration::microseconds(-1001).num_milliseconds(), Some(-1));
        assert_eq!(Duration::milliseconds(i32::MAX).num_milliseconds(), Some(i32::MAX as i64));
        assert_eq!(Duration::milliseconds(i32::MIN).num_milliseconds(), Some(i32::MIN as i64));
        assert_eq!(MAX.num_milliseconds(), None);
        assert_eq!(MIN.num_milliseconds(), None);
    }

    #[test]
    fn test_duration_num_microseconds() {
        let d: Duration = Zero::zero();
        assert_eq!(d.num_microseconds(), Some(0));
        assert_eq!(Duration::microseconds(1).num_microseconds(), Some(1));
        assert_eq!(Duration::microseconds(-1).num_microseconds(), Some(-1));
        assert_eq!(Duration::nanoseconds(999).num_microseconds(), Some(0));
        assert_eq!(Duration::nanoseconds(1001).num_microseconds(), Some(1));
        assert_eq!(Duration::nanoseconds(-999).num_microseconds(), Some(0));
        assert_eq!(Duration::nanoseconds(-1001).num_microseconds(), Some(-1));
        assert_eq!(Duration::microseconds(i32::MAX).num_microseconds(), Some(i32::MAX as i64));
        assert_eq!(Duration::microseconds(i32::MIN).num_microseconds(), Some(i32::MIN as i64));
        assert_eq!(MAX.num_microseconds(), None);
        assert_eq!(MIN.num_microseconds(), None);

        // overflow checks
        static MICROS_PER_DAY: i64 = 86400_000_000;
        assert_eq!(Duration::days((i64::MAX / MICROS_PER_DAY) as i32).num_microseconds(),
                   Some(i64::MAX / MICROS_PER_DAY * MICROS_PER_DAY));
        assert_eq!(Duration::days((i64::MIN / MICROS_PER_DAY) as i32).num_microseconds(),
                   Some(i64::MIN / MICROS_PER_DAY * MICROS_PER_DAY));
        assert_eq!(Duration::days((i64::MAX / MICROS_PER_DAY + 1) as i32).num_microseconds(), None);
        assert_eq!(Duration::days((i64::MIN / MICROS_PER_DAY - 1) as i32).num_microseconds(), None);
    }

    #[test]
    fn test_duration_num_nanoseconds() {
        let d: Duration = Zero::zero();
        assert_eq!(d.num_nanoseconds(), Some(0));
        assert_eq!(Duration::nanoseconds(1).num_nanoseconds(), Some(1));
        assert_eq!(Duration::nanoseconds(-1).num_nanoseconds(), Some(-1));
        assert_eq!(Duration::nanoseconds(i32::MAX).num_nanoseconds(), Some(i32::MAX as i64));
        assert_eq!(Duration::nanoseconds(i32::MIN).num_nanoseconds(), Some(i32::MIN as i64));
        assert_eq!(MAX.num_nanoseconds(), None);
        assert_eq!(MIN.num_nanoseconds(), None);

        // overflow checks
        static NANOS_PER_DAY: i64 = 86400_000_000_000;
        assert_eq!(Duration::days((i64::MAX / NANOS_PER_DAY) as i32).num_nanoseconds(),
                   Some(i64::MAX / NANOS_PER_DAY * NANOS_PER_DAY));
        assert_eq!(Duration::days((i64::MIN / NANOS_PER_DAY) as i32).num_nanoseconds(),
                   Some(i64::MIN / NANOS_PER_DAY * NANOS_PER_DAY));
        assert_eq!(Duration::days((i64::MAX / NANOS_PER_DAY + 1) as i32).num_nanoseconds(), None);
        assert_eq!(Duration::days((i64::MIN / NANOS_PER_DAY - 1) as i32).num_nanoseconds(), None);
    }

    #[test]
    fn test_duration_checked_ops() {
        assert_eq!(Duration::days(i32::MAX).checked_add(&Duration::seconds(86399)),
                   Some(Duration::days(i32::MAX - 1) + Duration::seconds(86400+86399)));
        assert_eq!(Duration::days(i32::MIN).checked_sub(&Duration::seconds(0)),
                   Some(Duration::days(i32::MIN)));
    }

    #[test]
    fn test_duration_mul() {
        let d: Duration = Zero::zero();
        assert_eq!(d * i32::MAX, d);
        assert_eq!(d * i32::MIN, d);
        assert_eq!(Duration::nanoseconds(1) * 0, Zero::zero());
        assert_eq!(Duration::nanoseconds(1) * 1, Duration::nanoseconds(1));
        assert_eq!(Duration::nanoseconds(1) * 1_000_000_000, Duration::seconds(1));
        assert_eq!(Duration::nanoseconds(1) * -1_000_000_000, -Duration::seconds(1));
        assert_eq!(-Duration::nanoseconds(1) * 1_000_000_000, -Duration::seconds(1));
        assert_eq!(Duration::nanoseconds(30) * 333_333_333,
                   Duration::seconds(10) - Duration::nanoseconds(10));
        assert_eq!((Duration::nanoseconds(1) + Duration::seconds(1) + Duration::days(1)) * 3,
                   Duration::nanoseconds(3) + Duration::seconds(3) + Duration::days(3));
    }

    #[test]
    fn test_duration_div() {
        let d: Duration = Zero::zero();
        assert_eq!(d / i32::MAX, d);
        assert_eq!(d / i32::MIN, d);
        assert_eq!(Duration::nanoseconds(123_456_789) / 1, Duration::nanoseconds(123_456_789));
        assert_eq!(Duration::nanoseconds(123_456_789) / -1, -Duration::nanoseconds(123_456_789));
        assert_eq!(-Duration::nanoseconds(123_456_789) / -1, Duration::nanoseconds(123_456_789));
        assert_eq!(-Duration::nanoseconds(123_456_789) / 1, -Duration::nanoseconds(123_456_789));
    }

    #[test]
    fn test_duration_fmt() {
        let d: Duration = Zero::zero();
        assert_eq!(d.to_string(), "PT0S".to_string());
        assert_eq!(Duration::days(42).to_string(), "P42D".to_string());
        assert_eq!(Duration::days(-42).to_string(), "P-42D".to_string());
        assert_eq!(Duration::seconds(42).to_string(), "PT42S".to_string());
        assert_eq!(Duration::milliseconds(42).to_string(), "PT0.042S".to_string());
        assert_eq!(Duration::microseconds(42).to_string(), "PT0.000042S".to_string());
        assert_eq!(Duration::nanoseconds(42).to_string(), "PT0.000000042S".to_string());
        assert_eq!((Duration::days(7) + Duration::milliseconds(6543)).to_string(),
                   "P7DT6.543S".to_string());

        // the format specifier should have no effect on `Duration`
        assert_eq!(format!("{:30}", Duration::days(1) + Duration::milliseconds(2345)),
                   "P1DT2.345S".to_string());
    }
}
