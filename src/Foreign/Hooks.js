"use strict";
exports.__esModule = true;
exports.useIntersectionObserver_ = void 0;
var react_1 = require("react");
function useIntersectionObserver_(elementRef) {
    var _a;
    var _b = (0, react_1.useState)(), entry = _b[0], setEntry = _b[1];
    // const frozen = entry?.isIntersecting && freezeOnceVisible;
    var updateEntry = function (_a) {
        var entry = _a[0];
        setEntry(entry);
    };
    (0, react_1.useEffect)(function () {
        var node = elementRef === null || elementRef === void 0 ? void 0 : elementRef.current; // DOM Ref
        var hasIOSupport = !!window.IntersectionObserver;
        if (!hasIOSupport || !node)
            return;
        var observerParams = {
            threshold: 0,
            root: null,
            rootMargin: "0%"
        };
        var observer = new IntersectionObserver(updateEntry, observerParams);
        observer.observe(node);
        return function () { return observer.disconnect(); };
    }, [elementRef === null || elementRef === void 0 ? void 0 : elementRef.current]);
    return (_a = entry === null || entry === void 0 ? void 0 : entry.isIntersecting) !== null && _a !== void 0 ? _a : false;
}
exports.useIntersectionObserver_ = useIntersectionObserver_;
