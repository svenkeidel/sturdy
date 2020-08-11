/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
import { Platform } from '@angular/cdk/platform';
import { Inject, Injectable, Optional } from '@angular/core';
import { DateAdapter, MAT_DATE_LOCALE } from './date-adapter';
// TODO(mmalerba): Remove when we no longer support safari 9.
/** Whether the browser supports the Intl API. */
let SUPPORTS_INTL_API;
// We need a try/catch around the reference to `Intl`, because accessing it in some cases can
// cause IE to throw. These cases are tied to particular versions of Windows and can happen if
// the consumer is providing a polyfilled `Map`. See:
// https://github.com/Microsoft/ChakraCore/issues/3189
// https://github.com/angular/components/issues/15687
try {
    SUPPORTS_INTL_API = typeof Intl != 'undefined';
}
catch (_a) {
    SUPPORTS_INTL_API = false;
}
/** The default month names to use if Intl API is not available. */
const DEFAULT_MONTH_NAMES = {
    'long': [
        'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September',
        'October', 'November', 'December'
    ],
    'short': ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'],
    'narrow': ['J', 'F', 'M', 'A', 'M', 'J', 'J', 'A', 'S', 'O', 'N', 'D']
};
const ɵ0 = i => String(i + 1);
/** The default date names to use if Intl API is not available. */
const DEFAULT_DATE_NAMES = range(31, ɵ0);
/** The default day of the week names to use if Intl API is not available. */
const DEFAULT_DAY_OF_WEEK_NAMES = {
    'long': ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'],
    'short': ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'],
    'narrow': ['S', 'M', 'T', 'W', 'T', 'F', 'S']
};
/**
 * Matches strings that have the form of a valid RFC 3339 string
 * (https://tools.ietf.org/html/rfc3339). Note that the string may not actually be a valid date
 * because the regex will match strings an with out of bounds month, date, etc.
 */
const ISO_8601_REGEX = /^\d{4}-\d{2}-\d{2}(?:T\d{2}:\d{2}:\d{2}(?:\.\d+)?(?:Z|(?:(?:\+|-)\d{2}:\d{2}))?)?$/;
/** Creates an array and fills it with values. */
function range(length, valueFunction) {
    const valuesArray = Array(length);
    for (let i = 0; i < length; i++) {
        valuesArray[i] = valueFunction(i);
    }
    return valuesArray;
}
/** Adapts the native JS Date for use with cdk-based components that work with dates. */
let NativeDateAdapter = /** @class */ (() => {
    class NativeDateAdapter extends DateAdapter {
        constructor(matDateLocale, platform) {
            super();
            /**
             * Whether to use `timeZone: 'utc'` with `Intl.DateTimeFormat` when formatting dates.
             * Without this `Intl.DateTimeFormat` sometimes chooses the wrong timeZone, which can throw off
             * the result. (e.g. in the en-US locale `new Date(1800, 7, 14).toLocaleDateString()`
             * will produce `'8/13/1800'`.
             *
             * TODO(mmalerba): drop this variable. It's not being used in the code right now. We're now
             * getting the string representation of a Date object from its utc representation. We're keeping
             * it here for sometime, just for precaution, in case we decide to revert some of these changes
             * though.
             */
            this.useUtcForDisplay = true;
            super.setLocale(matDateLocale);
            // IE does its own time zone correction, so we disable this on IE.
            this.useUtcForDisplay = !platform.TRIDENT;
            this._clampDate = platform.TRIDENT || platform.EDGE;
        }
        getYear(date) {
            return date.getFullYear();
        }
        getMonth(date) {
            return date.getMonth();
        }
        getDate(date) {
            return date.getDate();
        }
        getDayOfWeek(date) {
            return date.getDay();
        }
        getMonthNames(style) {
            if (SUPPORTS_INTL_API) {
                const dtf = new Intl.DateTimeFormat(this.locale, { month: style, timeZone: 'utc' });
                return range(12, i => this._stripDirectionalityCharacters(this._format(dtf, new Date(2017, i, 1))));
            }
            return DEFAULT_MONTH_NAMES[style];
        }
        getDateNames() {
            if (SUPPORTS_INTL_API) {
                const dtf = new Intl.DateTimeFormat(this.locale, { day: 'numeric', timeZone: 'utc' });
                return range(31, i => this._stripDirectionalityCharacters(this._format(dtf, new Date(2017, 0, i + 1))));
            }
            return DEFAULT_DATE_NAMES;
        }
        getDayOfWeekNames(style) {
            if (SUPPORTS_INTL_API) {
                const dtf = new Intl.DateTimeFormat(this.locale, { weekday: style, timeZone: 'utc' });
                return range(7, i => this._stripDirectionalityCharacters(this._format(dtf, new Date(2017, 0, i + 1))));
            }
            return DEFAULT_DAY_OF_WEEK_NAMES[style];
        }
        getYearName(date) {
            if (SUPPORTS_INTL_API) {
                const dtf = new Intl.DateTimeFormat(this.locale, { year: 'numeric', timeZone: 'utc' });
                return this._stripDirectionalityCharacters(this._format(dtf, date));
            }
            return String(this.getYear(date));
        }
        getFirstDayOfWeek() {
            // We can't tell using native JS Date what the first day of the week is, we default to Sunday.
            return 0;
        }
        getNumDaysInMonth(date) {
            return this.getDate(this._createDateWithOverflow(this.getYear(date), this.getMonth(date) + 1, 0));
        }
        clone(date) {
            return new Date(date.getTime());
        }
        createDate(year, month, date) {
            // Check for invalid month and date (except upper bound on date which we have to check after
            // creating the Date).
            if (month < 0 || month > 11) {
                throw Error(`Invalid month index "${month}". Month index has to be between 0 and 11.`);
            }
            if (date < 1) {
                throw Error(`Invalid date "${date}". Date has to be greater than 0.`);
            }
            let result = this._createDateWithOverflow(year, month, date);
            // Check that the date wasn't above the upper bound for the month, causing the month to overflow
            if (result.getMonth() != month) {
                throw Error(`Invalid date "${date}" for month with index "${month}".`);
            }
            return result;
        }
        today() {
            return new Date();
        }
        parse(value) {
            // We have no way using the native JS Date to set the parse format or locale, so we ignore these
            // parameters.
            if (typeof value == 'number') {
                return new Date(value);
            }
            return value ? new Date(Date.parse(value)) : null;
        }
        format(date, displayFormat) {
            if (!this.isValid(date)) {
                throw Error('NativeDateAdapter: Cannot format invalid date.');
            }
            if (SUPPORTS_INTL_API) {
                // On IE and Edge the i18n API will throw a hard error that can crash the entire app
                // if we attempt to format a date whose year is less than 1 or greater than 9999.
                if (this._clampDate && (date.getFullYear() < 1 || date.getFullYear() > 9999)) {
                    date = this.clone(date);
                    date.setFullYear(Math.max(1, Math.min(9999, date.getFullYear())));
                }
                displayFormat = Object.assign(Object.assign({}, displayFormat), { timeZone: 'utc' });
                const dtf = new Intl.DateTimeFormat(this.locale, displayFormat);
                return this._stripDirectionalityCharacters(this._format(dtf, date));
            }
            return this._stripDirectionalityCharacters(date.toDateString());
        }
        addCalendarYears(date, years) {
            return this.addCalendarMonths(date, years * 12);
        }
        addCalendarMonths(date, months) {
            let newDate = this._createDateWithOverflow(this.getYear(date), this.getMonth(date) + months, this.getDate(date));
            // It's possible to wind up in the wrong month if the original month has more days than the new
            // month. In this case we want to go to the last day of the desired month.
            // Note: the additional + 12 % 12 ensures we end up with a positive number, since JS % doesn't
            // guarantee this.
            if (this.getMonth(newDate) != ((this.getMonth(date) + months) % 12 + 12) % 12) {
                newDate = this._createDateWithOverflow(this.getYear(newDate), this.getMonth(newDate), 0);
            }
            return newDate;
        }
        addCalendarDays(date, days) {
            return this._createDateWithOverflow(this.getYear(date), this.getMonth(date), this.getDate(date) + days);
        }
        toIso8601(date) {
            return [
                date.getUTCFullYear(),
                this._2digit(date.getUTCMonth() + 1),
                this._2digit(date.getUTCDate())
            ].join('-');
        }
        /**
         * Returns the given value if given a valid Date or null. Deserializes valid ISO 8601 strings
         * (https://www.ietf.org/rfc/rfc3339.txt) into valid Dates and empty string into null. Returns an
         * invalid date for all other values.
         */
        deserialize(value) {
            if (typeof value === 'string') {
                if (!value) {
                    return null;
                }
                // The `Date` constructor accepts formats other than ISO 8601, so we need to make sure the
                // string is the right format first.
                if (ISO_8601_REGEX.test(value)) {
                    let date = new Date(value);
                    if (this.isValid(date)) {
                        return date;
                    }
                }
            }
            return super.deserialize(value);
        }
        isDateInstance(obj) {
            return obj instanceof Date;
        }
        isValid(date) {
            return !isNaN(date.getTime());
        }
        invalid() {
            return new Date(NaN);
        }
        /** Creates a date but allows the month and date to overflow. */
        _createDateWithOverflow(year, month, date) {
            const result = new Date(year, month, date);
            // We need to correct for the fact that JS native Date treats years in range [0, 99] as
            // abbreviations for 19xx.
            if (year >= 0 && year < 100) {
                result.setFullYear(this.getYear(result) - 1900);
            }
            return result;
        }
        /**
         * Pads a number to make it two digits.
         * @param n The number to pad.
         * @returns The padded number.
         */
        _2digit(n) {
            return ('00' + n).slice(-2);
        }
        /**
         * Strip out unicode LTR and RTL characters. Edge and IE insert these into formatted dates while
         * other browsers do not. We remove them to make output consistent and because they interfere with
         * date parsing.
         * @param str The string to strip direction characters from.
         * @returns The stripped string.
         */
        _stripDirectionalityCharacters(str) {
            return str.replace(/[\u200e\u200f]/g, '');
        }
        /**
         * When converting Date object to string, javascript built-in functions may return wrong
         * results because it applies its internal DST rules. The DST rules around the world change
         * very frequently, and the current valid rule is not always valid in previous years though.
         * We work around this problem building a new Date object which has its internal UTC
         * representation with the local date and time.
         * @param dtf Intl.DateTimeFormat object, containg the desired string format. It must have
         *    timeZone set to 'utc' to work fine.
         * @param date Date from which we want to get the string representation according to dtf
         * @returns A Date object with its UTC representation based on the passed in date info
         */
        _format(dtf, date) {
            const d = new Date(Date.UTC(date.getFullYear(), date.getMonth(), date.getDate(), date.getHours(), date.getMinutes(), date.getSeconds(), date.getMilliseconds()));
            return dtf.format(d);
        }
    }
    NativeDateAdapter.decorators = [
        { type: Injectable }
    ];
    NativeDateAdapter.ctorParameters = () => [
        { type: String, decorators: [{ type: Optional }, { type: Inject, args: [MAT_DATE_LOCALE,] }] },
        { type: Platform }
    ];
    return NativeDateAdapter;
})();
export { NativeDateAdapter };
export { ɵ0 };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibmF0aXZlLWRhdGUtYWRhcHRlci5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uLy4uL3NyYy9tYXRlcmlhbC9jb3JlL2RhdGV0aW1lL25hdGl2ZS1kYXRlLWFkYXB0ZXIudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HO0FBRUgsT0FBTyxFQUFDLFFBQVEsRUFBQyxNQUFNLHVCQUF1QixDQUFDO0FBQy9DLE9BQU8sRUFBQyxNQUFNLEVBQUUsVUFBVSxFQUFFLFFBQVEsRUFBQyxNQUFNLGVBQWUsQ0FBQztBQUMzRCxPQUFPLEVBQUMsV0FBVyxFQUFFLGVBQWUsRUFBQyxNQUFNLGdCQUFnQixDQUFDO0FBRTVELDZEQUE2RDtBQUM3RCxpREFBaUQ7QUFDakQsSUFBSSxpQkFBMEIsQ0FBQztBQUUvQiw2RkFBNkY7QUFDN0YsOEZBQThGO0FBQzlGLHFEQUFxRDtBQUNyRCxzREFBc0Q7QUFDdEQscURBQXFEO0FBQ3JELElBQUk7SUFDRixpQkFBaUIsR0FBRyxPQUFPLElBQUksSUFBSSxXQUFXLENBQUM7Q0FDaEQ7QUFBQyxXQUFNO0lBQ04saUJBQWlCLEdBQUcsS0FBSyxDQUFDO0NBQzNCO0FBRUQsbUVBQW1FO0FBQ25FLE1BQU0sbUJBQW1CLEdBQUc7SUFDMUIsTUFBTSxFQUFFO1FBQ04sU0FBUyxFQUFFLFVBQVUsRUFBRSxPQUFPLEVBQUUsT0FBTyxFQUFFLEtBQUssRUFBRSxNQUFNLEVBQUUsTUFBTSxFQUFFLFFBQVEsRUFBRSxXQUFXO1FBQ3JGLFNBQVMsRUFBRSxVQUFVLEVBQUUsVUFBVTtLQUNsQztJQUNELE9BQU8sRUFBRSxDQUFDLEtBQUssRUFBRSxLQUFLLEVBQUUsS0FBSyxFQUFFLEtBQUssRUFBRSxLQUFLLEVBQUUsS0FBSyxFQUFFLEtBQUssRUFBRSxLQUFLLEVBQUUsS0FBSyxFQUFFLEtBQUssRUFBRSxLQUFLLEVBQUUsS0FBSyxDQUFDO0lBQzdGLFFBQVEsRUFBRSxDQUFDLEdBQUcsRUFBRSxHQUFHLEVBQUUsR0FBRyxFQUFFLEdBQUcsRUFBRSxHQUFHLEVBQUUsR0FBRyxFQUFFLEdBQUcsRUFBRSxHQUFHLEVBQUUsR0FBRyxFQUFFLEdBQUcsRUFBRSxHQUFHLEVBQUUsR0FBRyxDQUFDO0NBQ3ZFLENBQUM7V0FJbUMsQ0FBQyxDQUFDLEVBQUUsQ0FBQyxNQUFNLENBQUMsQ0FBQyxHQUFHLENBQUMsQ0FBQztBQUR2RCxrRUFBa0U7QUFDbEUsTUFBTSxrQkFBa0IsR0FBRyxLQUFLLENBQUMsRUFBRSxLQUFxQixDQUFDO0FBR3pELDZFQUE2RTtBQUM3RSxNQUFNLHlCQUF5QixHQUFHO0lBQ2hDLE1BQU0sRUFBRSxDQUFDLFFBQVEsRUFBRSxRQUFRLEVBQUUsU0FBUyxFQUFFLFdBQVcsRUFBRSxVQUFVLEVBQUUsUUFBUSxFQUFFLFVBQVUsQ0FBQztJQUN0RixPQUFPLEVBQUUsQ0FBQyxLQUFLLEVBQUUsS0FBSyxFQUFFLEtBQUssRUFBRSxLQUFLLEVBQUUsS0FBSyxFQUFFLEtBQUssRUFBRSxLQUFLLENBQUM7SUFDMUQsUUFBUSxFQUFFLENBQUMsR0FBRyxFQUFFLEdBQUcsRUFBRSxHQUFHLEVBQUUsR0FBRyxFQUFFLEdBQUcsRUFBRSxHQUFHLEVBQUUsR0FBRyxDQUFDO0NBQzlDLENBQUM7QUFHRjs7OztHQUlHO0FBQ0gsTUFBTSxjQUFjLEdBQ2hCLG9GQUFvRixDQUFDO0FBR3pGLGlEQUFpRDtBQUNqRCxTQUFTLEtBQUssQ0FBSSxNQUFjLEVBQUUsYUFBbUM7SUFDbkUsTUFBTSxXQUFXLEdBQUcsS0FBSyxDQUFDLE1BQU0sQ0FBQyxDQUFDO0lBQ2xDLEtBQUssSUFBSSxDQUFDLEdBQUcsQ0FBQyxFQUFFLENBQUMsR0FBRyxNQUFNLEVBQUUsQ0FBQyxFQUFFLEVBQUU7UUFDL0IsV0FBVyxDQUFDLENBQUMsQ0FBQyxHQUFHLGFBQWEsQ0FBQyxDQUFDLENBQUMsQ0FBQztLQUNuQztJQUNELE9BQU8sV0FBVyxDQUFDO0FBQ3JCLENBQUM7QUFFRCx3RkFBd0Y7QUFDeEY7SUFBQSxNQUNhLGlCQUFrQixTQUFRLFdBQWlCO1FBaUJ0RCxZQUFpRCxhQUFxQixFQUFFLFFBQWtCO1lBQ3hGLEtBQUssRUFBRSxDQUFDO1lBZFY7Ozs7Ozs7Ozs7ZUFVRztZQUNILHFCQUFnQixHQUFZLElBQUksQ0FBQztZQUkvQixLQUFLLENBQUMsU0FBUyxDQUFDLGFBQWEsQ0FBQyxDQUFDO1lBRS9CLGtFQUFrRTtZQUNsRSxJQUFJLENBQUMsZ0JBQWdCLEdBQUcsQ0FBQyxRQUFRLENBQUMsT0FBTyxDQUFDO1lBQzFDLElBQUksQ0FBQyxVQUFVLEdBQUcsUUFBUSxDQUFDLE9BQU8sSUFBSSxRQUFRLENBQUMsSUFBSSxDQUFDO1FBQ3RELENBQUM7UUFFRCxPQUFPLENBQUMsSUFBVTtZQUNoQixPQUFPLElBQUksQ0FBQyxXQUFXLEVBQUUsQ0FBQztRQUM1QixDQUFDO1FBRUQsUUFBUSxDQUFDLElBQVU7WUFDakIsT0FBTyxJQUFJLENBQUMsUUFBUSxFQUFFLENBQUM7UUFDekIsQ0FBQztRQUVELE9BQU8sQ0FBQyxJQUFVO1lBQ2hCLE9BQU8sSUFBSSxDQUFDLE9BQU8sRUFBRSxDQUFDO1FBQ3hCLENBQUM7UUFFRCxZQUFZLENBQUMsSUFBVTtZQUNyQixPQUFPLElBQUksQ0FBQyxNQUFNLEVBQUUsQ0FBQztRQUN2QixDQUFDO1FBRUQsYUFBYSxDQUFDLEtBQWtDO1lBQzlDLElBQUksaUJBQWlCLEVBQUU7Z0JBQ3JCLE1BQU0sR0FBRyxHQUFHLElBQUksSUFBSSxDQUFDLGNBQWMsQ0FBQyxJQUFJLENBQUMsTUFBTSxFQUFFLEVBQUMsS0FBSyxFQUFFLEtBQUssRUFBRSxRQUFRLEVBQUUsS0FBSyxFQUFDLENBQUMsQ0FBQztnQkFDbEYsT0FBTyxLQUFLLENBQUMsRUFBRSxFQUFFLENBQUMsQ0FBQyxFQUFFLENBQ2pCLElBQUksQ0FBQyw4QkFBOEIsQ0FBQyxJQUFJLENBQUMsT0FBTyxDQUFDLEdBQUcsRUFBRSxJQUFJLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQyxFQUFFLENBQUMsQ0FBQyxDQUFDLENBQUMsQ0FBQyxDQUFDO2FBQ25GO1lBQ0QsT0FBTyxtQkFBbUIsQ0FBQyxLQUFLLENBQUMsQ0FBQztRQUNwQyxDQUFDO1FBRUQsWUFBWTtZQUNWLElBQUksaUJBQWlCLEVBQUU7Z0JBQ3JCLE1BQU0sR0FBRyxHQUFHLElBQUksSUFBSSxDQUFDLGNBQWMsQ0FBQyxJQUFJLENBQUMsTUFBTSxFQUFFLEVBQUMsR0FBRyxFQUFFLFNBQVMsRUFBRSxRQUFRLEVBQUUsS0FBSyxFQUFDLENBQUMsQ0FBQztnQkFDcEYsT0FBTyxLQUFLLENBQUMsRUFBRSxFQUFFLENBQUMsQ0FBQyxFQUFFLENBQUMsSUFBSSxDQUFDLDhCQUE4QixDQUNyRCxJQUFJLENBQUMsT0FBTyxDQUFDLEdBQUcsRUFBRSxJQUFJLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQyxFQUFFLENBQUMsR0FBRyxDQUFDLENBQUMsQ0FBQyxDQUFDLENBQUMsQ0FBQzthQUNuRDtZQUNELE9BQU8sa0JBQWtCLENBQUM7UUFDNUIsQ0FBQztRQUVELGlCQUFpQixDQUFDLEtBQWtDO1lBQ2xELElBQUksaUJBQWlCLEVBQUU7Z0JBQ3JCLE1BQU0sR0FBRyxHQUFHLElBQUksSUFBSSxDQUFDLGNBQWMsQ0FBQyxJQUFJLENBQUMsTUFBTSxFQUFFLEVBQUMsT0FBTyxFQUFFLEtBQUssRUFBRSxRQUFRLEVBQUUsS0FBSyxFQUFDLENBQUMsQ0FBQztnQkFDcEYsT0FBTyxLQUFLLENBQUMsQ0FBQyxFQUFFLENBQUMsQ0FBQyxFQUFFLENBQUMsSUFBSSxDQUFDLDhCQUE4QixDQUNwRCxJQUFJLENBQUMsT0FBTyxDQUFDLEdBQUcsRUFBRSxJQUFJLElBQUksQ0FBQyxJQUFJLEVBQUUsQ0FBQyxFQUFFLENBQUMsR0FBRyxDQUFDLENBQUMsQ0FBQyxDQUFDLENBQUMsQ0FBQzthQUNuRDtZQUNELE9BQU8seUJBQXlCLENBQUMsS0FBSyxDQUFDLENBQUM7UUFDMUMsQ0FBQztRQUVELFdBQVcsQ0FBQyxJQUFVO1lBQ3BCLElBQUksaUJBQWlCLEVBQUU7Z0JBQ3JCLE1BQU0sR0FBRyxHQUFHLElBQUksSUFBSSxDQUFDLGNBQWMsQ0FBQyxJQUFJLENBQUMsTUFBTSxFQUFFLEVBQUMsSUFBSSxFQUFFLFNBQVMsRUFBRSxRQUFRLEVBQUUsS0FBSyxFQUFDLENBQUMsQ0FBQztnQkFDckYsT0FBTyxJQUFJLENBQUMsOEJBQThCLENBQUMsSUFBSSxDQUFDLE9BQU8sQ0FBQyxHQUFHLEVBQUUsSUFBSSxDQUFDLENBQUMsQ0FBQzthQUNyRTtZQUNELE9BQU8sTUFBTSxDQUFDLElBQUksQ0FBQyxPQUFPLENBQUMsSUFBSSxDQUFDLENBQUMsQ0FBQztRQUNwQyxDQUFDO1FBRUQsaUJBQWlCO1lBQ2YsOEZBQThGO1lBQzlGLE9BQU8sQ0FBQyxDQUFDO1FBQ1gsQ0FBQztRQUVELGlCQUFpQixDQUFDLElBQVU7WUFDMUIsT0FBTyxJQUFJLENBQUMsT0FBTyxDQUFDLElBQUksQ0FBQyx1QkFBdUIsQ0FDNUMsSUFBSSxDQUFDLE9BQU8sQ0FBQyxJQUFJLENBQUMsRUFBRSxJQUFJLENBQUMsUUFBUSxDQUFDLElBQUksQ0FBQyxHQUFHLENBQUMsRUFBRSxDQUFDLENBQUMsQ0FBQyxDQUFDO1FBQ3ZELENBQUM7UUFFRCxLQUFLLENBQUMsSUFBVTtZQUNkLE9BQU8sSUFBSSxJQUFJLENBQUMsSUFBSSxDQUFDLE9BQU8sRUFBRSxDQUFDLENBQUM7UUFDbEMsQ0FBQztRQUVELFVBQVUsQ0FBQyxJQUFZLEVBQUUsS0FBYSxFQUFFLElBQVk7WUFDbEQsNEZBQTRGO1lBQzVGLHNCQUFzQjtZQUN0QixJQUFJLEtBQUssR0FBRyxDQUFDLElBQUksS0FBSyxHQUFHLEVBQUUsRUFBRTtnQkFDM0IsTUFBTSxLQUFLLENBQUMsd0JBQXdCLEtBQUssNENBQTRDLENBQUMsQ0FBQzthQUN4RjtZQUVELElBQUksSUFBSSxHQUFHLENBQUMsRUFBRTtnQkFDWixNQUFNLEtBQUssQ0FBQyxpQkFBaUIsSUFBSSxtQ0FBbUMsQ0FBQyxDQUFDO2FBQ3ZFO1lBRUQsSUFBSSxNQUFNLEdBQUcsSUFBSSxDQUFDLHVCQUF1QixDQUFDLElBQUksRUFBRSxLQUFLLEVBQUUsSUFBSSxDQUFDLENBQUM7WUFDN0QsZ0dBQWdHO1lBQ2hHLElBQUksTUFBTSxDQUFDLFFBQVEsRUFBRSxJQUFJLEtBQUssRUFBRTtnQkFDOUIsTUFBTSxLQUFLLENBQUMsaUJBQWlCLElBQUksMkJBQTJCLEtBQUssSUFBSSxDQUFDLENBQUM7YUFDeEU7WUFFRCxPQUFPLE1BQU0sQ0FBQztRQUNoQixDQUFDO1FBRUQsS0FBSztZQUNILE9BQU8sSUFBSSxJQUFJLEVBQUUsQ0FBQztRQUNwQixDQUFDO1FBRUQsS0FBSyxDQUFDLEtBQVU7WUFDZCxnR0FBZ0c7WUFDaEcsY0FBYztZQUNkLElBQUksT0FBTyxLQUFLLElBQUksUUFBUSxFQUFFO2dCQUM1QixPQUFPLElBQUksSUFBSSxDQUFDLEtBQUssQ0FBQyxDQUFDO2FBQ3hCO1lBQ0QsT0FBTyxLQUFLLENBQUMsQ0FBQyxDQUFDLElBQUksSUFBSSxDQUFDLElBQUksQ0FBQyxLQUFLLENBQUMsS0FBSyxDQUFDLENBQUMsQ0FBQyxDQUFDLENBQUMsSUFBSSxDQUFDO1FBQ3BELENBQUM7UUFFRCxNQUFNLENBQUMsSUFBVSxFQUFFLGFBQXFCO1lBQ3RDLElBQUksQ0FBQyxJQUFJLENBQUMsT0FBTyxDQUFDLElBQUksQ0FBQyxFQUFFO2dCQUN2QixNQUFNLEtBQUssQ0FBQyxnREFBZ0QsQ0FBQyxDQUFDO2FBQy9EO1lBRUQsSUFBSSxpQkFBaUIsRUFBRTtnQkFDckIsb0ZBQW9GO2dCQUNwRixpRkFBaUY7Z0JBQ2pGLElBQUksSUFBSSxDQUFDLFVBQVUsSUFBSSxDQUFDLElBQUksQ0FBQyxXQUFXLEVBQUUsR0FBRyxDQUFDLElBQUksSUFBSSxDQUFDLFdBQVcsRUFBRSxHQUFHLElBQUksQ0FBQyxFQUFFO29CQUM1RSxJQUFJLEdBQUcsSUFBSSxDQUFDLEtBQUssQ0FBQyxJQUFJLENBQUMsQ0FBQztvQkFDeEIsSUFBSSxDQUFDLFdBQVcsQ0FBQyxJQUFJLENBQUMsR0FBRyxDQUFDLENBQUMsRUFBRSxJQUFJLENBQUMsR0FBRyxDQUFDLElBQUksRUFBRSxJQUFJLENBQUMsV0FBVyxFQUFFLENBQUMsQ0FBQyxDQUFDLENBQUM7aUJBQ25FO2dCQUVELGFBQWEsbUNBQU8sYUFBYSxLQUFFLFFBQVEsRUFBRSxLQUFLLEdBQUMsQ0FBQztnQkFFcEQsTUFBTSxHQUFHLEdBQUcsSUFBSSxJQUFJLENBQUMsY0FBYyxDQUFDLElBQUksQ0FBQyxNQUFNLEVBQUUsYUFBYSxDQUFDLENBQUM7Z0JBQ2hFLE9BQU8sSUFBSSxDQUFDLDhCQUE4QixDQUFDLElBQUksQ0FBQyxPQUFPLENBQUMsR0FBRyxFQUFFLElBQUksQ0FBQyxDQUFDLENBQUM7YUFDckU7WUFDRCxPQUFPLElBQUksQ0FBQyw4QkFBOEIsQ0FBQyxJQUFJLENBQUMsWUFBWSxFQUFFLENBQUMsQ0FBQztRQUNsRSxDQUFDO1FBRUQsZ0JBQWdCLENBQUMsSUFBVSxFQUFFLEtBQWE7WUFDeEMsT0FBTyxJQUFJLENBQUMsaUJBQWlCLENBQUMsSUFBSSxFQUFFLEtBQUssR0FBRyxFQUFFLENBQUMsQ0FBQztRQUNsRCxDQUFDO1FBRUQsaUJBQWlCLENBQUMsSUFBVSxFQUFFLE1BQWM7WUFDMUMsSUFBSSxPQUFPLEdBQUcsSUFBSSxDQUFDLHVCQUF1QixDQUN0QyxJQUFJLENBQUMsT0FBTyxDQUFDLElBQUksQ0FBQyxFQUFFLElBQUksQ0FBQyxRQUFRLENBQUMsSUFBSSxDQUFDLEdBQUcsTUFBTSxFQUFFLElBQUksQ0FBQyxPQUFPLENBQUMsSUFBSSxDQUFDLENBQUMsQ0FBQztZQUUxRSwrRkFBK0Y7WUFDL0YsMEVBQTBFO1lBQzFFLDhGQUE4RjtZQUM5RixrQkFBa0I7WUFDbEIsSUFBSSxJQUFJLENBQUMsUUFBUSxDQUFDLE9BQU8sQ0FBQyxJQUFJLENBQUMsQ0FBQyxJQUFJLENBQUMsUUFBUSxDQUFDLElBQUksQ0FBQyxHQUFHLE1BQU0sQ0FBQyxHQUFHLEVBQUUsR0FBRyxFQUFFLENBQUMsR0FBRyxFQUFFLEVBQUU7Z0JBQzdFLE9BQU8sR0FBRyxJQUFJLENBQUMsdUJBQXVCLENBQUMsSUFBSSxDQUFDLE9BQU8sQ0FBQyxPQUFPLENBQUMsRUFBRSxJQUFJLENBQUMsUUFBUSxDQUFDLE9BQU8sQ0FBQyxFQUFFLENBQUMsQ0FBQyxDQUFDO2FBQzFGO1lBRUQsT0FBTyxPQUFPLENBQUM7UUFDakIsQ0FBQztRQUVELGVBQWUsQ0FBQyxJQUFVLEVBQUUsSUFBWTtZQUN0QyxPQUFPLElBQUksQ0FBQyx1QkFBdUIsQ0FDL0IsSUFBSSxDQUFDLE9BQU8sQ0FBQyxJQUFJLENBQUMsRUFBRSxJQUFJLENBQUMsUUFBUSxDQUFDLElBQUksQ0FBQyxFQUFFLElBQUksQ0FBQyxPQUFPLENBQUMsSUFBSSxDQUFDLEdBQUcsSUFBSSxDQUFDLENBQUM7UUFDMUUsQ0FBQztRQUVELFNBQVMsQ0FBQyxJQUFVO1lBQ2xCLE9BQU87Z0JBQ0wsSUFBSSxDQUFDLGNBQWMsRUFBRTtnQkFDckIsSUFBSSxDQUFDLE9BQU8sQ0FBQyxJQUFJLENBQUMsV0FBVyxFQUFFLEdBQUcsQ0FBQyxDQUFDO2dCQUNwQyxJQUFJLENBQUMsT0FBTyxDQUFDLElBQUksQ0FBQyxVQUFVLEVBQUUsQ0FBQzthQUNoQyxDQUFDLElBQUksQ0FBQyxHQUFHLENBQUMsQ0FBQztRQUNkLENBQUM7UUFFRDs7OztXQUlHO1FBQ0gsV0FBVyxDQUFDLEtBQVU7WUFDcEIsSUFBSSxPQUFPLEtBQUssS0FBSyxRQUFRLEVBQUU7Z0JBQzdCLElBQUksQ0FBQyxLQUFLLEVBQUU7b0JBQ1YsT0FBTyxJQUFJLENBQUM7aUJBQ2I7Z0JBQ0QsMEZBQTBGO2dCQUMxRixvQ0FBb0M7Z0JBQ3BDLElBQUksY0FBYyxDQUFDLElBQUksQ0FBQyxLQUFLLENBQUMsRUFBRTtvQkFDOUIsSUFBSSxJQUFJLEdBQUcsSUFBSSxJQUFJLENBQUMsS0FBSyxDQUFDLENBQUM7b0JBQzNCLElBQUksSUFBSSxDQUFDLE9BQU8sQ0FBQyxJQUFJLENBQUMsRUFBRTt3QkFDdEIsT0FBTyxJQUFJLENBQUM7cUJBQ2I7aUJBQ0Y7YUFDRjtZQUNELE9BQU8sS0FBSyxDQUFDLFdBQVcsQ0FBQyxLQUFLLENBQUMsQ0FBQztRQUNsQyxDQUFDO1FBRUQsY0FBYyxDQUFDLEdBQVE7WUFDckIsT0FBTyxHQUFHLFlBQVksSUFBSSxDQUFDO1FBQzdCLENBQUM7UUFFRCxPQUFPLENBQUMsSUFBVTtZQUNoQixPQUFPLENBQUMsS0FBSyxDQUFDLElBQUksQ0FBQyxPQUFPLEVBQUUsQ0FBQyxDQUFDO1FBQ2hDLENBQUM7UUFFRCxPQUFPO1lBQ0wsT0FBTyxJQUFJLElBQUksQ0FBQyxHQUFHLENBQUMsQ0FBQztRQUN2QixDQUFDO1FBRUQsZ0VBQWdFO1FBQ3hELHVCQUF1QixDQUFDLElBQVksRUFBRSxLQUFhLEVBQUUsSUFBWTtZQUN2RSxNQUFNLE1BQU0sR0FBRyxJQUFJLElBQUksQ0FBQyxJQUFJLEVBQUUsS0FBSyxFQUFFLElBQUksQ0FBQyxDQUFDO1lBRTNDLHVGQUF1RjtZQUN2RiwwQkFBMEI7WUFDMUIsSUFBSSxJQUFJLElBQUksQ0FBQyxJQUFJLElBQUksR0FBRyxHQUFHLEVBQUU7Z0JBQzNCLE1BQU0sQ0FBQyxXQUFXLENBQUMsSUFBSSxDQUFDLE9BQU8sQ0FBQyxNQUFNLENBQUMsR0FBRyxJQUFJLENBQUMsQ0FBQzthQUNqRDtZQUNELE9BQU8sTUFBTSxDQUFDO1FBQ2hCLENBQUM7UUFFRDs7OztXQUlHO1FBQ0ssT0FBTyxDQUFDLENBQVM7WUFDdkIsT0FBTyxDQUFDLElBQUksR0FBRyxDQUFDLENBQUMsQ0FBQyxLQUFLLENBQUMsQ0FBQyxDQUFDLENBQUMsQ0FBQztRQUM5QixDQUFDO1FBRUQ7Ozs7OztXQU1HO1FBQ0ssOEJBQThCLENBQUMsR0FBVztZQUNoRCxPQUFPLEdBQUcsQ0FBQyxPQUFPLENBQUMsaUJBQWlCLEVBQUUsRUFBRSxDQUFDLENBQUM7UUFDNUMsQ0FBQztRQUVEOzs7Ozs7Ozs7O1dBVUc7UUFDSyxPQUFPLENBQUMsR0FBd0IsRUFBRSxJQUFVO1lBQ2xELE1BQU0sQ0FBQyxHQUFHLElBQUksSUFBSSxDQUFDLElBQUksQ0FBQyxHQUFHLENBQ3ZCLElBQUksQ0FBQyxXQUFXLEVBQUUsRUFBRSxJQUFJLENBQUMsUUFBUSxFQUFFLEVBQUUsSUFBSSxDQUFDLE9BQU8sRUFBRSxFQUFFLElBQUksQ0FBQyxRQUFRLEVBQUUsRUFDcEUsSUFBSSxDQUFDLFVBQVUsRUFBRSxFQUFFLElBQUksQ0FBQyxVQUFVLEVBQUUsRUFBRSxJQUFJLENBQUMsZUFBZSxFQUFFLENBQUMsQ0FBQyxDQUFDO1lBQ25FLE9BQU8sR0FBRyxDQUFDLE1BQU0sQ0FBQyxDQUFDLENBQUMsQ0FBQztRQUN2QixDQUFDOzs7Z0JBcFFGLFVBQVU7Ozs2Q0FrQkksUUFBUSxZQUFJLE1BQU0sU0FBQyxlQUFlO2dCQS9FekMsUUFBUTs7SUFrVWhCLHdCQUFDO0tBQUE7U0FwUVksaUJBQWlCIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7UGxhdGZvcm19IGZyb20gJ0Bhbmd1bGFyL2Nkay9wbGF0Zm9ybSc7XG5pbXBvcnQge0luamVjdCwgSW5qZWN0YWJsZSwgT3B0aW9uYWx9IGZyb20gJ0Bhbmd1bGFyL2NvcmUnO1xuaW1wb3J0IHtEYXRlQWRhcHRlciwgTUFUX0RBVEVfTE9DQUxFfSBmcm9tICcuL2RhdGUtYWRhcHRlcic7XG5cbi8vIFRPRE8obW1hbGVyYmEpOiBSZW1vdmUgd2hlbiB3ZSBubyBsb25nZXIgc3VwcG9ydCBzYWZhcmkgOS5cbi8qKiBXaGV0aGVyIHRoZSBicm93c2VyIHN1cHBvcnRzIHRoZSBJbnRsIEFQSS4gKi9cbmxldCBTVVBQT1JUU19JTlRMX0FQSTogYm9vbGVhbjtcblxuLy8gV2UgbmVlZCBhIHRyeS9jYXRjaCBhcm91bmQgdGhlIHJlZmVyZW5jZSB0byBgSW50bGAsIGJlY2F1c2UgYWNjZXNzaW5nIGl0IGluIHNvbWUgY2FzZXMgY2FuXG4vLyBjYXVzZSBJRSB0byB0aHJvdy4gVGhlc2UgY2FzZXMgYXJlIHRpZWQgdG8gcGFydGljdWxhciB2ZXJzaW9ucyBvZiBXaW5kb3dzIGFuZCBjYW4gaGFwcGVuIGlmXG4vLyB0aGUgY29uc3VtZXIgaXMgcHJvdmlkaW5nIGEgcG9seWZpbGxlZCBgTWFwYC4gU2VlOlxuLy8gaHR0cHM6Ly9naXRodWIuY29tL01pY3Jvc29mdC9DaGFrcmFDb3JlL2lzc3Vlcy8zMTg5XG4vLyBodHRwczovL2dpdGh1Yi5jb20vYW5ndWxhci9jb21wb25lbnRzL2lzc3Vlcy8xNTY4N1xudHJ5IHtcbiAgU1VQUE9SVFNfSU5UTF9BUEkgPSB0eXBlb2YgSW50bCAhPSAndW5kZWZpbmVkJztcbn0gY2F0Y2gge1xuICBTVVBQT1JUU19JTlRMX0FQSSA9IGZhbHNlO1xufVxuXG4vKiogVGhlIGRlZmF1bHQgbW9udGggbmFtZXMgdG8gdXNlIGlmIEludGwgQVBJIGlzIG5vdCBhdmFpbGFibGUuICovXG5jb25zdCBERUZBVUxUX01PTlRIX05BTUVTID0ge1xuICAnbG9uZyc6IFtcbiAgICAnSmFudWFyeScsICdGZWJydWFyeScsICdNYXJjaCcsICdBcHJpbCcsICdNYXknLCAnSnVuZScsICdKdWx5JywgJ0F1Z3VzdCcsICdTZXB0ZW1iZXInLFxuICAgICdPY3RvYmVyJywgJ05vdmVtYmVyJywgJ0RlY2VtYmVyJ1xuICBdLFxuICAnc2hvcnQnOiBbJ0phbicsICdGZWInLCAnTWFyJywgJ0FwcicsICdNYXknLCAnSnVuJywgJ0p1bCcsICdBdWcnLCAnU2VwJywgJ09jdCcsICdOb3YnLCAnRGVjJ10sXG4gICduYXJyb3cnOiBbJ0onLCAnRicsICdNJywgJ0EnLCAnTScsICdKJywgJ0onLCAnQScsICdTJywgJ08nLCAnTicsICdEJ11cbn07XG5cblxuLyoqIFRoZSBkZWZhdWx0IGRhdGUgbmFtZXMgdG8gdXNlIGlmIEludGwgQVBJIGlzIG5vdCBhdmFpbGFibGUuICovXG5jb25zdCBERUZBVUxUX0RBVEVfTkFNRVMgPSByYW5nZSgzMSwgaSA9PiBTdHJpbmcoaSArIDEpKTtcblxuXG4vKiogVGhlIGRlZmF1bHQgZGF5IG9mIHRoZSB3ZWVrIG5hbWVzIHRvIHVzZSBpZiBJbnRsIEFQSSBpcyBub3QgYXZhaWxhYmxlLiAqL1xuY29uc3QgREVGQVVMVF9EQVlfT0ZfV0VFS19OQU1FUyA9IHtcbiAgJ2xvbmcnOiBbJ1N1bmRheScsICdNb25kYXknLCAnVHVlc2RheScsICdXZWRuZXNkYXknLCAnVGh1cnNkYXknLCAnRnJpZGF5JywgJ1NhdHVyZGF5J10sXG4gICdzaG9ydCc6IFsnU3VuJywgJ01vbicsICdUdWUnLCAnV2VkJywgJ1RodScsICdGcmknLCAnU2F0J10sXG4gICduYXJyb3cnOiBbJ1MnLCAnTScsICdUJywgJ1cnLCAnVCcsICdGJywgJ1MnXVxufTtcblxuXG4vKipcbiAqIE1hdGNoZXMgc3RyaW5ncyB0aGF0IGhhdmUgdGhlIGZvcm0gb2YgYSB2YWxpZCBSRkMgMzMzOSBzdHJpbmdcbiAqIChodHRwczovL3Rvb2xzLmlldGYub3JnL2h0bWwvcmZjMzMzOSkuIE5vdGUgdGhhdCB0aGUgc3RyaW5nIG1heSBub3QgYWN0dWFsbHkgYmUgYSB2YWxpZCBkYXRlXG4gKiBiZWNhdXNlIHRoZSByZWdleCB3aWxsIG1hdGNoIHN0cmluZ3MgYW4gd2l0aCBvdXQgb2YgYm91bmRzIG1vbnRoLCBkYXRlLCBldGMuXG4gKi9cbmNvbnN0IElTT184NjAxX1JFR0VYID1cbiAgICAvXlxcZHs0fS1cXGR7Mn0tXFxkezJ9KD86VFxcZHsyfTpcXGR7Mn06XFxkezJ9KD86XFwuXFxkKyk/KD86WnwoPzooPzpcXCt8LSlcXGR7Mn06XFxkezJ9KSk/KT8kLztcblxuXG4vKiogQ3JlYXRlcyBhbiBhcnJheSBhbmQgZmlsbHMgaXQgd2l0aCB2YWx1ZXMuICovXG5mdW5jdGlvbiByYW5nZTxUPihsZW5ndGg6IG51bWJlciwgdmFsdWVGdW5jdGlvbjogKGluZGV4OiBudW1iZXIpID0+IFQpOiBUW10ge1xuICBjb25zdCB2YWx1ZXNBcnJheSA9IEFycmF5KGxlbmd0aCk7XG4gIGZvciAobGV0IGkgPSAwOyBpIDwgbGVuZ3RoOyBpKyspIHtcbiAgICB2YWx1ZXNBcnJheVtpXSA9IHZhbHVlRnVuY3Rpb24oaSk7XG4gIH1cbiAgcmV0dXJuIHZhbHVlc0FycmF5O1xufVxuXG4vKiogQWRhcHRzIHRoZSBuYXRpdmUgSlMgRGF0ZSBmb3IgdXNlIHdpdGggY2RrLWJhc2VkIGNvbXBvbmVudHMgdGhhdCB3b3JrIHdpdGggZGF0ZXMuICovXG5ASW5qZWN0YWJsZSgpXG5leHBvcnQgY2xhc3MgTmF0aXZlRGF0ZUFkYXB0ZXIgZXh0ZW5kcyBEYXRlQWRhcHRlcjxEYXRlPiB7XG4gIC8qKiBXaGV0aGVyIHRvIGNsYW1wIHRoZSBkYXRlIGJldHdlZW4gMSBhbmQgOTk5OSB0byBhdm9pZCBJRSBhbmQgRWRnZSBlcnJvcnMuICovXG4gIHByaXZhdGUgcmVhZG9ubHkgX2NsYW1wRGF0ZTogYm9vbGVhbjtcblxuICAvKipcbiAgICogV2hldGhlciB0byB1c2UgYHRpbWVab25lOiAndXRjJ2Agd2l0aCBgSW50bC5EYXRlVGltZUZvcm1hdGAgd2hlbiBmb3JtYXR0aW5nIGRhdGVzLlxuICAgKiBXaXRob3V0IHRoaXMgYEludGwuRGF0ZVRpbWVGb3JtYXRgIHNvbWV0aW1lcyBjaG9vc2VzIHRoZSB3cm9uZyB0aW1lWm9uZSwgd2hpY2ggY2FuIHRocm93IG9mZlxuICAgKiB0aGUgcmVzdWx0LiAoZS5nLiBpbiB0aGUgZW4tVVMgbG9jYWxlIGBuZXcgRGF0ZSgxODAwLCA3LCAxNCkudG9Mb2NhbGVEYXRlU3RyaW5nKClgXG4gICAqIHdpbGwgcHJvZHVjZSBgJzgvMTMvMTgwMCdgLlxuICAgKlxuICAgKiBUT0RPKG1tYWxlcmJhKTogZHJvcCB0aGlzIHZhcmlhYmxlLiBJdCdzIG5vdCBiZWluZyB1c2VkIGluIHRoZSBjb2RlIHJpZ2h0IG5vdy4gV2UncmUgbm93XG4gICAqIGdldHRpbmcgdGhlIHN0cmluZyByZXByZXNlbnRhdGlvbiBvZiBhIERhdGUgb2JqZWN0IGZyb20gaXRzIHV0YyByZXByZXNlbnRhdGlvbi4gV2UncmUga2VlcGluZ1xuICAgKiBpdCBoZXJlIGZvciBzb21ldGltZSwganVzdCBmb3IgcHJlY2F1dGlvbiwgaW4gY2FzZSB3ZSBkZWNpZGUgdG8gcmV2ZXJ0IHNvbWUgb2YgdGhlc2UgY2hhbmdlc1xuICAgKiB0aG91Z2guXG4gICAqL1xuICB1c2VVdGNGb3JEaXNwbGF5OiBib29sZWFuID0gdHJ1ZTtcblxuICBjb25zdHJ1Y3RvcihAT3B0aW9uYWwoKSBASW5qZWN0KE1BVF9EQVRFX0xPQ0FMRSkgbWF0RGF0ZUxvY2FsZTogc3RyaW5nLCBwbGF0Zm9ybTogUGxhdGZvcm0pIHtcbiAgICBzdXBlcigpO1xuICAgIHN1cGVyLnNldExvY2FsZShtYXREYXRlTG9jYWxlKTtcblxuICAgIC8vIElFIGRvZXMgaXRzIG93biB0aW1lIHpvbmUgY29ycmVjdGlvbiwgc28gd2UgZGlzYWJsZSB0aGlzIG9uIElFLlxuICAgIHRoaXMudXNlVXRjRm9yRGlzcGxheSA9ICFwbGF0Zm9ybS5UUklERU5UO1xuICAgIHRoaXMuX2NsYW1wRGF0ZSA9IHBsYXRmb3JtLlRSSURFTlQgfHwgcGxhdGZvcm0uRURHRTtcbiAgfVxuXG4gIGdldFllYXIoZGF0ZTogRGF0ZSk6IG51bWJlciB7XG4gICAgcmV0dXJuIGRhdGUuZ2V0RnVsbFllYXIoKTtcbiAgfVxuXG4gIGdldE1vbnRoKGRhdGU6IERhdGUpOiBudW1iZXIge1xuICAgIHJldHVybiBkYXRlLmdldE1vbnRoKCk7XG4gIH1cblxuICBnZXREYXRlKGRhdGU6IERhdGUpOiBudW1iZXIge1xuICAgIHJldHVybiBkYXRlLmdldERhdGUoKTtcbiAgfVxuXG4gIGdldERheU9mV2VlayhkYXRlOiBEYXRlKTogbnVtYmVyIHtcbiAgICByZXR1cm4gZGF0ZS5nZXREYXkoKTtcbiAgfVxuXG4gIGdldE1vbnRoTmFtZXMoc3R5bGU6ICdsb25nJyB8ICdzaG9ydCcgfCAnbmFycm93Jyk6IHN0cmluZ1tdIHtcbiAgICBpZiAoU1VQUE9SVFNfSU5UTF9BUEkpIHtcbiAgICAgIGNvbnN0IGR0ZiA9IG5ldyBJbnRsLkRhdGVUaW1lRm9ybWF0KHRoaXMubG9jYWxlLCB7bW9udGg6IHN0eWxlLCB0aW1lWm9uZTogJ3V0Yyd9KTtcbiAgICAgIHJldHVybiByYW5nZSgxMiwgaSA9PlxuICAgICAgICAgIHRoaXMuX3N0cmlwRGlyZWN0aW9uYWxpdHlDaGFyYWN0ZXJzKHRoaXMuX2Zvcm1hdChkdGYsIG5ldyBEYXRlKDIwMTcsIGksIDEpKSkpO1xuICAgIH1cbiAgICByZXR1cm4gREVGQVVMVF9NT05USF9OQU1FU1tzdHlsZV07XG4gIH1cblxuICBnZXREYXRlTmFtZXMoKTogc3RyaW5nW10ge1xuICAgIGlmIChTVVBQT1JUU19JTlRMX0FQSSkge1xuICAgICAgY29uc3QgZHRmID0gbmV3IEludGwuRGF0ZVRpbWVGb3JtYXQodGhpcy5sb2NhbGUsIHtkYXk6ICdudW1lcmljJywgdGltZVpvbmU6ICd1dGMnfSk7XG4gICAgICByZXR1cm4gcmFuZ2UoMzEsIGkgPT4gdGhpcy5fc3RyaXBEaXJlY3Rpb25hbGl0eUNoYXJhY3RlcnMoXG4gICAgICAgICAgdGhpcy5fZm9ybWF0KGR0ZiwgbmV3IERhdGUoMjAxNywgMCwgaSArIDEpKSkpO1xuICAgIH1cbiAgICByZXR1cm4gREVGQVVMVF9EQVRFX05BTUVTO1xuICB9XG5cbiAgZ2V0RGF5T2ZXZWVrTmFtZXMoc3R5bGU6ICdsb25nJyB8ICdzaG9ydCcgfCAnbmFycm93Jyk6IHN0cmluZ1tdIHtcbiAgICBpZiAoU1VQUE9SVFNfSU5UTF9BUEkpIHtcbiAgICAgIGNvbnN0IGR0ZiA9IG5ldyBJbnRsLkRhdGVUaW1lRm9ybWF0KHRoaXMubG9jYWxlLCB7d2Vla2RheTogc3R5bGUsIHRpbWVab25lOiAndXRjJ30pO1xuICAgICAgcmV0dXJuIHJhbmdlKDcsIGkgPT4gdGhpcy5fc3RyaXBEaXJlY3Rpb25hbGl0eUNoYXJhY3RlcnMoXG4gICAgICAgICAgdGhpcy5fZm9ybWF0KGR0ZiwgbmV3IERhdGUoMjAxNywgMCwgaSArIDEpKSkpO1xuICAgIH1cbiAgICByZXR1cm4gREVGQVVMVF9EQVlfT0ZfV0VFS19OQU1FU1tzdHlsZV07XG4gIH1cblxuICBnZXRZZWFyTmFtZShkYXRlOiBEYXRlKTogc3RyaW5nIHtcbiAgICBpZiAoU1VQUE9SVFNfSU5UTF9BUEkpIHtcbiAgICAgIGNvbnN0IGR0ZiA9IG5ldyBJbnRsLkRhdGVUaW1lRm9ybWF0KHRoaXMubG9jYWxlLCB7eWVhcjogJ251bWVyaWMnLCB0aW1lWm9uZTogJ3V0Yyd9KTtcbiAgICAgIHJldHVybiB0aGlzLl9zdHJpcERpcmVjdGlvbmFsaXR5Q2hhcmFjdGVycyh0aGlzLl9mb3JtYXQoZHRmLCBkYXRlKSk7XG4gICAgfVxuICAgIHJldHVybiBTdHJpbmcodGhpcy5nZXRZZWFyKGRhdGUpKTtcbiAgfVxuXG4gIGdldEZpcnN0RGF5T2ZXZWVrKCk6IG51bWJlciB7XG4gICAgLy8gV2UgY2FuJ3QgdGVsbCB1c2luZyBuYXRpdmUgSlMgRGF0ZSB3aGF0IHRoZSBmaXJzdCBkYXkgb2YgdGhlIHdlZWsgaXMsIHdlIGRlZmF1bHQgdG8gU3VuZGF5LlxuICAgIHJldHVybiAwO1xuICB9XG5cbiAgZ2V0TnVtRGF5c0luTW9udGgoZGF0ZTogRGF0ZSk6IG51bWJlciB7XG4gICAgcmV0dXJuIHRoaXMuZ2V0RGF0ZSh0aGlzLl9jcmVhdGVEYXRlV2l0aE92ZXJmbG93KFxuICAgICAgICB0aGlzLmdldFllYXIoZGF0ZSksIHRoaXMuZ2V0TW9udGgoZGF0ZSkgKyAxLCAwKSk7XG4gIH1cblxuICBjbG9uZShkYXRlOiBEYXRlKTogRGF0ZSB7XG4gICAgcmV0dXJuIG5ldyBEYXRlKGRhdGUuZ2V0VGltZSgpKTtcbiAgfVxuXG4gIGNyZWF0ZURhdGUoeWVhcjogbnVtYmVyLCBtb250aDogbnVtYmVyLCBkYXRlOiBudW1iZXIpOiBEYXRlIHtcbiAgICAvLyBDaGVjayBmb3IgaW52YWxpZCBtb250aCBhbmQgZGF0ZSAoZXhjZXB0IHVwcGVyIGJvdW5kIG9uIGRhdGUgd2hpY2ggd2UgaGF2ZSB0byBjaGVjayBhZnRlclxuICAgIC8vIGNyZWF0aW5nIHRoZSBEYXRlKS5cbiAgICBpZiAobW9udGggPCAwIHx8IG1vbnRoID4gMTEpIHtcbiAgICAgIHRocm93IEVycm9yKGBJbnZhbGlkIG1vbnRoIGluZGV4IFwiJHttb250aH1cIi4gTW9udGggaW5kZXggaGFzIHRvIGJlIGJldHdlZW4gMCBhbmQgMTEuYCk7XG4gICAgfVxuXG4gICAgaWYgKGRhdGUgPCAxKSB7XG4gICAgICB0aHJvdyBFcnJvcihgSW52YWxpZCBkYXRlIFwiJHtkYXRlfVwiLiBEYXRlIGhhcyB0byBiZSBncmVhdGVyIHRoYW4gMC5gKTtcbiAgICB9XG5cbiAgICBsZXQgcmVzdWx0ID0gdGhpcy5fY3JlYXRlRGF0ZVdpdGhPdmVyZmxvdyh5ZWFyLCBtb250aCwgZGF0ZSk7XG4gICAgLy8gQ2hlY2sgdGhhdCB0aGUgZGF0ZSB3YXNuJ3QgYWJvdmUgdGhlIHVwcGVyIGJvdW5kIGZvciB0aGUgbW9udGgsIGNhdXNpbmcgdGhlIG1vbnRoIHRvIG92ZXJmbG93XG4gICAgaWYgKHJlc3VsdC5nZXRNb250aCgpICE9IG1vbnRoKSB7XG4gICAgICB0aHJvdyBFcnJvcihgSW52YWxpZCBkYXRlIFwiJHtkYXRlfVwiIGZvciBtb250aCB3aXRoIGluZGV4IFwiJHttb250aH1cIi5gKTtcbiAgICB9XG5cbiAgICByZXR1cm4gcmVzdWx0O1xuICB9XG5cbiAgdG9kYXkoKTogRGF0ZSB7XG4gICAgcmV0dXJuIG5ldyBEYXRlKCk7XG4gIH1cblxuICBwYXJzZSh2YWx1ZTogYW55KTogRGF0ZSB8IG51bGwge1xuICAgIC8vIFdlIGhhdmUgbm8gd2F5IHVzaW5nIHRoZSBuYXRpdmUgSlMgRGF0ZSB0byBzZXQgdGhlIHBhcnNlIGZvcm1hdCBvciBsb2NhbGUsIHNvIHdlIGlnbm9yZSB0aGVzZVxuICAgIC8vIHBhcmFtZXRlcnMuXG4gICAgaWYgKHR5cGVvZiB2YWx1ZSA9PSAnbnVtYmVyJykge1xuICAgICAgcmV0dXJuIG5ldyBEYXRlKHZhbHVlKTtcbiAgICB9XG4gICAgcmV0dXJuIHZhbHVlID8gbmV3IERhdGUoRGF0ZS5wYXJzZSh2YWx1ZSkpIDogbnVsbDtcbiAgfVxuXG4gIGZvcm1hdChkYXRlOiBEYXRlLCBkaXNwbGF5Rm9ybWF0OiBPYmplY3QpOiBzdHJpbmcge1xuICAgIGlmICghdGhpcy5pc1ZhbGlkKGRhdGUpKSB7XG4gICAgICB0aHJvdyBFcnJvcignTmF0aXZlRGF0ZUFkYXB0ZXI6IENhbm5vdCBmb3JtYXQgaW52YWxpZCBkYXRlLicpO1xuICAgIH1cblxuICAgIGlmIChTVVBQT1JUU19JTlRMX0FQSSkge1xuICAgICAgLy8gT24gSUUgYW5kIEVkZ2UgdGhlIGkxOG4gQVBJIHdpbGwgdGhyb3cgYSBoYXJkIGVycm9yIHRoYXQgY2FuIGNyYXNoIHRoZSBlbnRpcmUgYXBwXG4gICAgICAvLyBpZiB3ZSBhdHRlbXB0IHRvIGZvcm1hdCBhIGRhdGUgd2hvc2UgeWVhciBpcyBsZXNzIHRoYW4gMSBvciBncmVhdGVyIHRoYW4gOTk5OS5cbiAgICAgIGlmICh0aGlzLl9jbGFtcERhdGUgJiYgKGRhdGUuZ2V0RnVsbFllYXIoKSA8IDEgfHwgZGF0ZS5nZXRGdWxsWWVhcigpID4gOTk5OSkpIHtcbiAgICAgICAgZGF0ZSA9IHRoaXMuY2xvbmUoZGF0ZSk7XG4gICAgICAgIGRhdGUuc2V0RnVsbFllYXIoTWF0aC5tYXgoMSwgTWF0aC5taW4oOTk5OSwgZGF0ZS5nZXRGdWxsWWVhcigpKSkpO1xuICAgICAgfVxuXG4gICAgICBkaXNwbGF5Rm9ybWF0ID0gey4uLmRpc3BsYXlGb3JtYXQsIHRpbWVab25lOiAndXRjJ307XG5cbiAgICAgIGNvbnN0IGR0ZiA9IG5ldyBJbnRsLkRhdGVUaW1lRm9ybWF0KHRoaXMubG9jYWxlLCBkaXNwbGF5Rm9ybWF0KTtcbiAgICAgIHJldHVybiB0aGlzLl9zdHJpcERpcmVjdGlvbmFsaXR5Q2hhcmFjdGVycyh0aGlzLl9mb3JtYXQoZHRmLCBkYXRlKSk7XG4gICAgfVxuICAgIHJldHVybiB0aGlzLl9zdHJpcERpcmVjdGlvbmFsaXR5Q2hhcmFjdGVycyhkYXRlLnRvRGF0ZVN0cmluZygpKTtcbiAgfVxuXG4gIGFkZENhbGVuZGFyWWVhcnMoZGF0ZTogRGF0ZSwgeWVhcnM6IG51bWJlcik6IERhdGUge1xuICAgIHJldHVybiB0aGlzLmFkZENhbGVuZGFyTW9udGhzKGRhdGUsIHllYXJzICogMTIpO1xuICB9XG5cbiAgYWRkQ2FsZW5kYXJNb250aHMoZGF0ZTogRGF0ZSwgbW9udGhzOiBudW1iZXIpOiBEYXRlIHtcbiAgICBsZXQgbmV3RGF0ZSA9IHRoaXMuX2NyZWF0ZURhdGVXaXRoT3ZlcmZsb3coXG4gICAgICAgIHRoaXMuZ2V0WWVhcihkYXRlKSwgdGhpcy5nZXRNb250aChkYXRlKSArIG1vbnRocywgdGhpcy5nZXREYXRlKGRhdGUpKTtcblxuICAgIC8vIEl0J3MgcG9zc2libGUgdG8gd2luZCB1cCBpbiB0aGUgd3JvbmcgbW9udGggaWYgdGhlIG9yaWdpbmFsIG1vbnRoIGhhcyBtb3JlIGRheXMgdGhhbiB0aGUgbmV3XG4gICAgLy8gbW9udGguIEluIHRoaXMgY2FzZSB3ZSB3YW50IHRvIGdvIHRvIHRoZSBsYXN0IGRheSBvZiB0aGUgZGVzaXJlZCBtb250aC5cbiAgICAvLyBOb3RlOiB0aGUgYWRkaXRpb25hbCArIDEyICUgMTIgZW5zdXJlcyB3ZSBlbmQgdXAgd2l0aCBhIHBvc2l0aXZlIG51bWJlciwgc2luY2UgSlMgJSBkb2Vzbid0XG4gICAgLy8gZ3VhcmFudGVlIHRoaXMuXG4gICAgaWYgKHRoaXMuZ2V0TW9udGgobmV3RGF0ZSkgIT0gKCh0aGlzLmdldE1vbnRoKGRhdGUpICsgbW9udGhzKSAlIDEyICsgMTIpICUgMTIpIHtcbiAgICAgIG5ld0RhdGUgPSB0aGlzLl9jcmVhdGVEYXRlV2l0aE92ZXJmbG93KHRoaXMuZ2V0WWVhcihuZXdEYXRlKSwgdGhpcy5nZXRNb250aChuZXdEYXRlKSwgMCk7XG4gICAgfVxuXG4gICAgcmV0dXJuIG5ld0RhdGU7XG4gIH1cblxuICBhZGRDYWxlbmRhckRheXMoZGF0ZTogRGF0ZSwgZGF5czogbnVtYmVyKTogRGF0ZSB7XG4gICAgcmV0dXJuIHRoaXMuX2NyZWF0ZURhdGVXaXRoT3ZlcmZsb3coXG4gICAgICAgIHRoaXMuZ2V0WWVhcihkYXRlKSwgdGhpcy5nZXRNb250aChkYXRlKSwgdGhpcy5nZXREYXRlKGRhdGUpICsgZGF5cyk7XG4gIH1cblxuICB0b0lzbzg2MDEoZGF0ZTogRGF0ZSk6IHN0cmluZyB7XG4gICAgcmV0dXJuIFtcbiAgICAgIGRhdGUuZ2V0VVRDRnVsbFllYXIoKSxcbiAgICAgIHRoaXMuXzJkaWdpdChkYXRlLmdldFVUQ01vbnRoKCkgKyAxKSxcbiAgICAgIHRoaXMuXzJkaWdpdChkYXRlLmdldFVUQ0RhdGUoKSlcbiAgICBdLmpvaW4oJy0nKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBSZXR1cm5zIHRoZSBnaXZlbiB2YWx1ZSBpZiBnaXZlbiBhIHZhbGlkIERhdGUgb3IgbnVsbC4gRGVzZXJpYWxpemVzIHZhbGlkIElTTyA4NjAxIHN0cmluZ3NcbiAgICogKGh0dHBzOi8vd3d3LmlldGYub3JnL3JmYy9yZmMzMzM5LnR4dCkgaW50byB2YWxpZCBEYXRlcyBhbmQgZW1wdHkgc3RyaW5nIGludG8gbnVsbC4gUmV0dXJucyBhblxuICAgKiBpbnZhbGlkIGRhdGUgZm9yIGFsbCBvdGhlciB2YWx1ZXMuXG4gICAqL1xuICBkZXNlcmlhbGl6ZSh2YWx1ZTogYW55KTogRGF0ZSB8IG51bGwge1xuICAgIGlmICh0eXBlb2YgdmFsdWUgPT09ICdzdHJpbmcnKSB7XG4gICAgICBpZiAoIXZhbHVlKSB7XG4gICAgICAgIHJldHVybiBudWxsO1xuICAgICAgfVxuICAgICAgLy8gVGhlIGBEYXRlYCBjb25zdHJ1Y3RvciBhY2NlcHRzIGZvcm1hdHMgb3RoZXIgdGhhbiBJU08gODYwMSwgc28gd2UgbmVlZCB0byBtYWtlIHN1cmUgdGhlXG4gICAgICAvLyBzdHJpbmcgaXMgdGhlIHJpZ2h0IGZvcm1hdCBmaXJzdC5cbiAgICAgIGlmIChJU09fODYwMV9SRUdFWC50ZXN0KHZhbHVlKSkge1xuICAgICAgICBsZXQgZGF0ZSA9IG5ldyBEYXRlKHZhbHVlKTtcbiAgICAgICAgaWYgKHRoaXMuaXNWYWxpZChkYXRlKSkge1xuICAgICAgICAgIHJldHVybiBkYXRlO1xuICAgICAgICB9XG4gICAgICB9XG4gICAgfVxuICAgIHJldHVybiBzdXBlci5kZXNlcmlhbGl6ZSh2YWx1ZSk7XG4gIH1cblxuICBpc0RhdGVJbnN0YW5jZShvYmo6IGFueSkge1xuICAgIHJldHVybiBvYmogaW5zdGFuY2VvZiBEYXRlO1xuICB9XG5cbiAgaXNWYWxpZChkYXRlOiBEYXRlKSB7XG4gICAgcmV0dXJuICFpc05hTihkYXRlLmdldFRpbWUoKSk7XG4gIH1cblxuICBpbnZhbGlkKCk6IERhdGUge1xuICAgIHJldHVybiBuZXcgRGF0ZShOYU4pO1xuICB9XG5cbiAgLyoqIENyZWF0ZXMgYSBkYXRlIGJ1dCBhbGxvd3MgdGhlIG1vbnRoIGFuZCBkYXRlIHRvIG92ZXJmbG93LiAqL1xuICBwcml2YXRlIF9jcmVhdGVEYXRlV2l0aE92ZXJmbG93KHllYXI6IG51bWJlciwgbW9udGg6IG51bWJlciwgZGF0ZTogbnVtYmVyKSB7XG4gICAgY29uc3QgcmVzdWx0ID0gbmV3IERhdGUoeWVhciwgbW9udGgsIGRhdGUpO1xuXG4gICAgLy8gV2UgbmVlZCB0byBjb3JyZWN0IGZvciB0aGUgZmFjdCB0aGF0IEpTIG5hdGl2ZSBEYXRlIHRyZWF0cyB5ZWFycyBpbiByYW5nZSBbMCwgOTldIGFzXG4gICAgLy8gYWJicmV2aWF0aW9ucyBmb3IgMTl4eC5cbiAgICBpZiAoeWVhciA+PSAwICYmIHllYXIgPCAxMDApIHtcbiAgICAgIHJlc3VsdC5zZXRGdWxsWWVhcih0aGlzLmdldFllYXIocmVzdWx0KSAtIDE5MDApO1xuICAgIH1cbiAgICByZXR1cm4gcmVzdWx0O1xuICB9XG5cbiAgLyoqXG4gICAqIFBhZHMgYSBudW1iZXIgdG8gbWFrZSBpdCB0d28gZGlnaXRzLlxuICAgKiBAcGFyYW0gbiBUaGUgbnVtYmVyIHRvIHBhZC5cbiAgICogQHJldHVybnMgVGhlIHBhZGRlZCBudW1iZXIuXG4gICAqL1xuICBwcml2YXRlIF8yZGlnaXQobjogbnVtYmVyKSB7XG4gICAgcmV0dXJuICgnMDAnICsgbikuc2xpY2UoLTIpO1xuICB9XG5cbiAgLyoqXG4gICAqIFN0cmlwIG91dCB1bmljb2RlIExUUiBhbmQgUlRMIGNoYXJhY3RlcnMuIEVkZ2UgYW5kIElFIGluc2VydCB0aGVzZSBpbnRvIGZvcm1hdHRlZCBkYXRlcyB3aGlsZVxuICAgKiBvdGhlciBicm93c2VycyBkbyBub3QuIFdlIHJlbW92ZSB0aGVtIHRvIG1ha2Ugb3V0cHV0IGNvbnNpc3RlbnQgYW5kIGJlY2F1c2UgdGhleSBpbnRlcmZlcmUgd2l0aFxuICAgKiBkYXRlIHBhcnNpbmcuXG4gICAqIEBwYXJhbSBzdHIgVGhlIHN0cmluZyB0byBzdHJpcCBkaXJlY3Rpb24gY2hhcmFjdGVycyBmcm9tLlxuICAgKiBAcmV0dXJucyBUaGUgc3RyaXBwZWQgc3RyaW5nLlxuICAgKi9cbiAgcHJpdmF0ZSBfc3RyaXBEaXJlY3Rpb25hbGl0eUNoYXJhY3RlcnMoc3RyOiBzdHJpbmcpIHtcbiAgICByZXR1cm4gc3RyLnJlcGxhY2UoL1tcXHUyMDBlXFx1MjAwZl0vZywgJycpO1xuICB9XG5cbiAgLyoqXG4gICAqIFdoZW4gY29udmVydGluZyBEYXRlIG9iamVjdCB0byBzdHJpbmcsIGphdmFzY3JpcHQgYnVpbHQtaW4gZnVuY3Rpb25zIG1heSByZXR1cm4gd3JvbmdcbiAgICogcmVzdWx0cyBiZWNhdXNlIGl0IGFwcGxpZXMgaXRzIGludGVybmFsIERTVCBydWxlcy4gVGhlIERTVCBydWxlcyBhcm91bmQgdGhlIHdvcmxkIGNoYW5nZVxuICAgKiB2ZXJ5IGZyZXF1ZW50bHksIGFuZCB0aGUgY3VycmVudCB2YWxpZCBydWxlIGlzIG5vdCBhbHdheXMgdmFsaWQgaW4gcHJldmlvdXMgeWVhcnMgdGhvdWdoLlxuICAgKiBXZSB3b3JrIGFyb3VuZCB0aGlzIHByb2JsZW0gYnVpbGRpbmcgYSBuZXcgRGF0ZSBvYmplY3Qgd2hpY2ggaGFzIGl0cyBpbnRlcm5hbCBVVENcbiAgICogcmVwcmVzZW50YXRpb24gd2l0aCB0aGUgbG9jYWwgZGF0ZSBhbmQgdGltZS5cbiAgICogQHBhcmFtIGR0ZiBJbnRsLkRhdGVUaW1lRm9ybWF0IG9iamVjdCwgY29udGFpbmcgdGhlIGRlc2lyZWQgc3RyaW5nIGZvcm1hdC4gSXQgbXVzdCBoYXZlXG4gICAqICAgIHRpbWVab25lIHNldCB0byAndXRjJyB0byB3b3JrIGZpbmUuXG4gICAqIEBwYXJhbSBkYXRlIERhdGUgZnJvbSB3aGljaCB3ZSB3YW50IHRvIGdldCB0aGUgc3RyaW5nIHJlcHJlc2VudGF0aW9uIGFjY29yZGluZyB0byBkdGZcbiAgICogQHJldHVybnMgQSBEYXRlIG9iamVjdCB3aXRoIGl0cyBVVEMgcmVwcmVzZW50YXRpb24gYmFzZWQgb24gdGhlIHBhc3NlZCBpbiBkYXRlIGluZm9cbiAgICovXG4gIHByaXZhdGUgX2Zvcm1hdChkdGY6IEludGwuRGF0ZVRpbWVGb3JtYXQsIGRhdGU6IERhdGUpIHtcbiAgICBjb25zdCBkID0gbmV3IERhdGUoRGF0ZS5VVEMoXG4gICAgICAgIGRhdGUuZ2V0RnVsbFllYXIoKSwgZGF0ZS5nZXRNb250aCgpLCBkYXRlLmdldERhdGUoKSwgZGF0ZS5nZXRIb3VycygpLFxuICAgICAgICBkYXRlLmdldE1pbnV0ZXMoKSwgZGF0ZS5nZXRTZWNvbmRzKCksIGRhdGUuZ2V0TWlsbGlzZWNvbmRzKCkpKTtcbiAgICByZXR1cm4gZHRmLmZvcm1hdChkKTtcbiAgfVxufVxuIl19