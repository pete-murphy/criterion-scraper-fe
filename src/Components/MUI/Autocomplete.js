"use strict";
var __makeTemplateObject = (this && this.__makeTemplateObject) || function (cooked, raw) {
    if (Object.defineProperty) { Object.defineProperty(cooked, "raw", { value: raw }); } else { cooked.raw = raw; }
    return cooked;
};
var __assign = (this && this.__assign) || function () {
    __assign = Object.assign || function(t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
            s = arguments[i];
            for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))
                t[p] = s[p];
        }
        return t;
    };
    return __assign.apply(this, arguments);
};
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    Object.defineProperty(o, k2, { enumerable: true, get: function() { return m[k]; } });
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (k !== "default" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
};
var __rest = (this && this.__rest) || function (s, e) {
    var t = {};
    for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p) && e.indexOf(p) < 0)
        t[p] = s[p];
    if (s != null && typeof Object.getOwnPropertySymbols === "function")
        for (var i = 0, p = Object.getOwnPropertySymbols(s); i < p.length; i++) {
            if (e.indexOf(p[i]) < 0 && Object.prototype.propertyIsEnumerable.call(s, p[i]))
                t[p[i]] = s[p[i]];
        }
    return t;
};
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
exports.__esModule = true;
exports.autocomplete_ = void 0;
var React = __importStar(require("react"));
var AutocompleteUnstyled_1 = require("@mui/base/AutocompleteUnstyled");
var Check_1 = __importDefault(require("@mui/icons-material/Check"));
var Close_1 = __importDefault(require("@mui/icons-material/Close"));
var styles_1 = require("@mui/material/styles");
var Root = (0, styles_1.styled)("div")(function (_a) {
    var theme = _a.theme;
    return "\n  color: white;\n  font-size: 14px;\n";
});
var Label = (0, styles_1.styled)("label")(templateObject_1 || (templateObject_1 = __makeTemplateObject(["\n  padding: 0 0 4px;\n  line-height: 1.5;\n  display: block;\n"], ["\n  padding: 0 0 4px;\n  line-height: 1.5;\n  display: block;\n"])));
var InputWrapper = (0, styles_1.styled)("div")(function (_a) {
    var theme = _a.theme;
    return "\n  width: 300px;\n  border: 1px solid ".concat(theme.palette.mode === "dark" ? "#434343" : "#d9d9d9", ";\n  border: 1px solid whitesmoke;\n  background-color: ").concat(theme.palette.mode === "dark" ? "#141414" : "#fff", ";\n  background-color: hsl(0, 1.6%, 12%);\n  border-radius: 4px;\n  padding: 1px;\n  display: flex;\n  flex-wrap: wrap;\n\n  &:hover {\n    border-color: ").concat(theme.palette.mode === "dark" ? "#177ddc" : "#40a9ff", ";\n    border-color: dodger-blue;\n  }\n\n  &.focused {\n    border-color: ").concat(theme.palette.mode === "dark" ? "#177ddc" : "#40a9ff", ";\n    border-color: dodger-blue;\n    box-shadow: 0 0 0 2px rgba(24, 144, 255, 0.2);\n  }\n\n  & input {\n    background-color: ").concat(theme.palette.mode === "dark" ? "#141414" : "#fff", ";\n    background-color: hsl(0, 1.6%, 12%);\n    color: ").concat(theme.palette.mode === "dark"
        ? "rgba(255,255,255,0.65)"
        : "rgba(0,0,0,.85)", ";\n    color: white;\n    height: 30px;\n    box-sizing: border-box;\n    padding: 4px 6px;\n    width: 0;\n    min-width: 30px;\n    flex-grow: 1;\n    border: 0;\n    margin: 0;\n    outline: 0;\n  }\n");
});
function Tag(props) {
    var label = props.label, onDelete = props.onDelete, other = __rest(props, ["label", "onDelete"]);
    return (React.createElement("div", __assign({}, other),
        React.createElement("span", null, label),
        React.createElement(Close_1["default"], { onClick: onDelete })));
}
var StyledTag = (0, styles_1.styled)(Tag)(function (_a) {
    var theme = _a.theme;
    return "\n  display: flex;\n  align-items: center;\n  height: 24px;\n  margin: 2px;\n  line-height: 22px;\n  background-color: ".concat(theme.palette.mode === "dark" ? "rgba(255,255,255,0.08)" : "#fafafa", ";\n  background-color: hsl(0, 1.6%, 20%);\n  border: 1px solid ").concat(theme.palette.mode === "dark" ? "#303030" : "#e8e8e8", ";\n  border: hsl(0, 1.6%, 12%);\n  border-radius: 2px;\n  box-sizing: content-box;\n  padding: 0 4px 0 10px;\n  outline: 0;\n  overflow: hidden;\n\n  &:focus {\n    border-color: ").concat(theme.palette.mode === "dark" ? "#177ddc" : "#40a9ff", ";\n    border-color: dodgerblue;\n    background-color: ").concat(theme.palette.mode === "dark" ? "#003b57" : "#e6f7ff", ";\n    background-color: hsl(0, 1.6%, 24%);\n  }\n\n  & span {\n    overflow: hidden;\n    white-space: nowrap;\n    text-overflow: ellipsis;\n  }\n\n  & svg {\n    font-size: 12px;\n    cursor: pointer;\n  }\n");
});
var Listbox = (0, styles_1.styled)("ul")(function (_a) {
    var theme = _a.theme;
    return "\n  width: 300px;\n  margin: 2px 0 0;\n  padding: 0;\n  position: absolute;\n  list-style: none;\n  background-color: ".concat(theme.palette.mode === "dark" ? "#141414" : "#fff", ";\n  background-color: hsl(0, 1.6%, 12%);\n  overflow: auto;\n  max-height: 250px;\n  border-radius: 4px;\n  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.15);\n  z-index: 1;\n\n  & li {\n    padding: 5px 12px;\n    display: flex;\n\n    & span {\n      flex-grow: 1;\n    }\n\n    & svg {\n      color: transparent;\n    }\n  }\n\n  & li[aria-selected='true'] {\n    background-color: ").concat(theme.palette.mode === "dark" ? "#2b2b2b" : "#fafafa", ";\n    background-color: hsl(0, 1.6%, 20%);\n    font-weight: 600;\n\n    & svg {\n      color: #1890ff;\n    }\n  }\n\n  & li[data-focus='true'], & .Mui-focused {\n    background-color: ").concat(theme.palette.mode === "dark" ? "#003b57" : "#e6f7ff", ";\n    background-color: hsl(0, 1.6%, 24%);\n    cursor: pointer;\n\n    & svg {\n      color: currentColor;\n    }\n  }\n");
});
// TODO: Don't fix this to string
function autocomplete_(props) {
    var _a;
    var getOptionLabel = (_a = props.getOptionLabel) !== null && _a !== void 0 ? _a : (function (x) { return x; });
    var _b = (0, AutocompleteUnstyled_1.useAutocomplete)({
        multiple: true,
        options: props.options,
        getOptionLabel: function (x) { return x; },
        onChange: function (_e, value) { return props.onChange(value); },
        value: props.value
    }), getRootProps = _b.getRootProps, getInputLabelProps = _b.getInputLabelProps, getInputProps = _b.getInputProps, getTagProps = _b.getTagProps, getListboxProps = _b.getListboxProps, getOptionProps = _b.getOptionProps, groupedOptions = _b.groupedOptions, value = _b.value, focused = _b.focused, setAnchorEl = _b.setAnchorEl;
    return (React.createElement(Root, null,
        React.createElement("div", __assign({}, getRootProps()),
            React.createElement(Label, __assign({}, getInputLabelProps()), props.label),
            React.createElement(InputWrapper, { ref: setAnchorEl, className: focused ? "focused" : "" },
                value.map(function (option, index) { return (React.createElement(StyledTag, __assign({ label: getOptionLabel(option) }, getTagProps({ index: index })))); }),
                React.createElement("input", __assign({}, getInputProps())))),
        groupedOptions.length > 0 ? (React.createElement(Listbox, __assign({}, getListboxProps()), groupedOptions.map(function (option, index) { return (React.createElement("li", __assign({}, getOptionProps({ option: option, index: index })),
            React.createElement("span", null, getOptionLabel(option)),
            React.createElement(Check_1["default"], { fontSize: "small" }))); }))) : null));
}
exports.autocomplete_ = autocomplete_;
var templateObject_1;
