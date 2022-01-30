"use strict";
exports.__esModule = true;
exports.Slider_ = void 0;
var React = require("react");
var SliderUnstyled_1 = require("@mui/base/SliderUnstyled");
var MUI = require("@mui/system");
var Slider = MUI.styled(SliderUnstyled_1["default"])(
  "\n  color: tomato;\n  height: 4px;\n  width: 100%;\n  padding: 13px 0;\n  display: inline-block;\n  position: relative;\n  cursor: pointer;\n  touch-action: none;\n  -webkit-tap-highlight-color: transparent;\n  opacity: 0.75;\n  &:hover, &:active {\n    opacity: 1;\n  }\n\n  & .MuiSlider-rail {\n    display: block;\n    position: absolute;\n    width: 100%;\n    height: 4px;\n    border-radius: 2px;\n    background-color: currentColor;\n    opacity: 0.38;\n  }\n\n  & .MuiSlider-track {\n    display: block;\n    position: absolute;\n    height: 4px;\n    border-radius: 2px;\n    background-color: currentColor;\n  }\n\n  & .MuiSlider-thumb {\n    position: absolute;\n    width: 14px;\n    height: 14px;\n    margin-left: -6px;\n    margin-top: -5px;\n    box-sizing: border-box;\n    border-radius: 50%;\n    outline: 0;\n    border: 2px solid currentColor;\n    background-color: #fff;\n\n    :hover,\n    &.Mui-focusVisible {\n      box-shadow: 0 0 0 0.25rem dodgerblue;\n    }\n\n    &.Mui-active {\n      box-shadow: 0 0 0 0.25rem dodgerblue;\n    }\n  }\n"
);
function Slider_(_a) {
  var value = _a.value,
    setValue = _a.setValue,
    minDistance = _a.minDistance,
    min = _a.min,
    max = _a.max;
  var onChange = function (_event, newValue, activeThumb) {
    if (!Array.isArray(newValue)) {
      return;
    }
    if (newValue[1] - newValue[0] < minDistance) {
      if (activeThumb === 0) {
        var clamped = Math.min(newValue[0], max - minDistance);
        setValue([clamped, clamped + minDistance]);
      } else {
        var clamped = Math.max(newValue[1], min + minDistance);
        setValue([clamped - minDistance, clamped]);
      }
    } else {
      setValue(newValue);
    }
  };
  return React.createElement(
    MUI.Box,
    { sx: { width: 300 } },
    React.createElement(Slider, {
      getAriaLabel: function () {
        return "Minimum distance shift";
      },
      value: value,
      onChange: onChange,
      valueLabelDisplay: "off",
      disableSwap: true,
      min: min,
      max: max,
    })
  );
}
exports.Slider_ = Slider_;
