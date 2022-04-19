"use strict";
exports.__esModule = true;
exports.useDebounce_ = void 0;
var use_debounce_1 = require("use-debounce");
var useDebounce_ = function (value, delay) {
    return (0, use_debounce_1.useDebounce)(value, delay)[0];
};
exports.useDebounce_ = useDebounce_;
