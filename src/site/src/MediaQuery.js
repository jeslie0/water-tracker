"use strict"

export function matchMedia (window) {
    return function (string) {
        return function () {
            return window.matchMedia(string);
        }
    }
}

export function matches(mediaQueryList) {
    return mediaQueryList.matches;
}
