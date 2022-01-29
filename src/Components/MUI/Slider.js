"use strict";

const React = require("react");
const SliderUnstyled = require("@mui/base/SliderUnstyled").default;
const { styled, Box } = require("@mui/system");

exports.slider_ = Slider_;

const Slider = styled(SliderUnstyled)(`
  color: tomato;
  height: 4px;
  width: 100%;
  padding: 13px 0;
  display: inline-block;
  position: relative;
  cursor: pointer;
  touch-action: none;
  -webkit-tap-highlight-color: transparent;
  opacity: 0.75;
  &:hover, &:active {
    opacity: 1;
  }

  & .MuiSlider-rail {
    display: block;
    position: absolute;
    width: 100%;
    height: 4px;
    border-radius: 2px;
    background-color: currentColor;
    opacity: 0.38;
  }

  & .MuiSlider-track {
    display: block;
    position: absolute;
    height: 4px;
    border-radius: 2px;
    background-color: currentColor;
  }

  & .MuiSlider-thumb {
    position: absolute;
    width: 14px;
    height: 14px;
    margin-left: -6px;
    margin-top: -5px;
    box-sizing: border-box;
    border-radius: 50%;
    outline: 0;
    border: 2px solid currentColor;
    background-color: #fff;

    :hover,
    &.Mui-focusVisible {
      box-shadow: 0 0 0 0.25rem dodgerblue;
    }

    &.Mui-active {
      box-shadow: 0 0 0 0.25rem dodgerblue;
    }
  }
`);

function Slider_({
  value, //       :: Array Int
  setValue, //    :: Array Int -> Effect Unit
  minDistance, // :: Int
  min, //         :: Int
  max, //         :: Int
}) {
  const onChange = (event, newValue, activeThumb) => {
    if (!Array.isArray(newValue)) {
      return;
    }

    if (newValue[1] - newValue[0] < minDistance) {
      if (activeThumb === 0) {
        const clamped = Math.min(newValue[0], max - minDistance);
        setValue([clamped, clamped + minDistance]);
      } else {
        const clamped = Math.max(newValue[1], min + minDistance);
        setValue([clamped - minDistance, clamped]);
      }
    } else {
      setValue(newValue);
    }
  };

  return React.createElement(Box, {
    sx: { width: 300 },
    children: [
      React.createElement(Slider, {
        getAriaLabel: function () {
          return "Minimum distance shift";
        },
        value: value,
        onChange: onChange,
        valueLabelDisplay: "off",
        // getAriaValueText:
        disableSwap: true,
        min: min,
        max: max,
      }),
    ],
  });
}

// exports.slider_ = _Slider;
