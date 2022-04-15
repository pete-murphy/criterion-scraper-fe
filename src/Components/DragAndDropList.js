"use strict";
var __extends = (this && this.__extends) || (function () {
    var extendStatics = function (d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (Object.prototype.hasOwnProperty.call(b, p)) d[p] = b[p]; };
        return extendStatics(d, b);
    };
    return function (d, b) {
        if (typeof b !== "function" && b !== null)
            throw new TypeError("Class extends value " + String(b) + " is not a constructor or null");
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
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
exports.__esModule = true;
exports.dragAndDropList_ = void 0;
var react_1 = __importStar(require("react"));
var react_beautiful_dnd_1 = require("react-beautiful-dnd");
// a little function to help us with reordering the result
var reorder = function (list, startIndex, endIndex) {
    var result = Array.from(list);
    var removed = result.splice(startIndex, 1)[0];
    result.splice(endIndex, 0, removed);
    return result;
};
var grid = 8;
var getItemStyle = function (isDragging, draggableStyle) { return (__assign({ 
    // some basic styles to make the items look a bit nicer
    userSelect: "none", 
    // padding: grid * 2,
    // margin: `0 0 ${grid}px 0`,
    // change background colour if dragging
    background: isDragging ? "tomato" : "inherit" }, draggableStyle)); };
var dragAndDropList_ = /** @class */ (function (_super) {
    __extends(dragAndDropList_, _super);
    function dragAndDropList_(props) {
        var _this = _super.call(this, props) || this;
        _this.onDragEnd = _this.onDragEnd.bind(_this);
        return _this;
    }
    dragAndDropList_.prototype.onDragEnd = function (result) {
        // dropped outside the list
        if (!result.destination) {
            return;
        }
        var items = reorder(this.props.items, result.source.index, result.destination.index);
        this.props.setItems(items);
    };
    // Normally you would want to split things out into separate components.
    // But in this example everything is just done in one place for simplicity
    dragAndDropList_.prototype.render = function () {
        var _this = this;
        return (
        // @ts-ignore
        react_1["default"].createElement(react_beautiful_dnd_1.DragDropContext, { onDragEnd: this.onDragEnd },
            react_1["default"].createElement(react_beautiful_dnd_1.Droppable, { droppableId: "droppable" }, function (provided, snapshot) { return (react_1["default"].createElement("ul", __assign({ className: "drag-ul" }, provided.droppableProps, { ref: provided.innerRef }),
                _this.props.items.map(function (item, index) { return (react_1["default"].createElement(react_beautiful_dnd_1.Draggable, { key: _this.props.keyForItem(item), draggableId: _this.props.keyForItem(item), index: index }, function (provided, snapshot) { return (react_1["default"].createElement("li", __assign({ ref: provided.innerRef }, provided.draggableProps, provided.dragHandleProps, { style: getItemStyle(snapshot.isDragging, provided.draggableProps.style), className: _this.props.isItemSelected(item) ? "selected" : "", onClick: function (_) {
                        _this.props.onClickItem(item);
                    } }), _this.props.renderItem(item))); })); }),
                provided.placeholder)); })));
    };
    return dragAndDropList_;
}(react_1.Component));
exports.dragAndDropList_ = dragAndDropList_;
