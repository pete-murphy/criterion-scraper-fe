import React, { Component, ReactElement } from "react";
import { DragDropContext, Droppable, Draggable } from "react-beautiful-dnd";

// a little function to help us with reordering the result
const reorder = (
  list: Iterable<unknown> | ArrayLike<unknown>,
  startIndex: number,
  endIndex: number
) => {
  const result = Array.from(list);
  const [removed] = result.splice(startIndex, 1);
  result.splice(endIndex, 0, removed);

  return result;
};

const grid = 8;

const getItemStyle = (isDragging: any, draggableStyle: any) => ({
  // some basic styles to make the items look a bit nicer
  userSelect: "none",
  // padding: grid * 2,
  // margin: `0 0 ${grid}px 0`,

  // change background colour if dragging
  background: isDragging ? "tomato" : "inherit",

  // styles we need to apply on draggables
  ...draggableStyle,
});

type Props<A> = {
  readonly items: Array<A>;
  readonly renderItem: (_: A) => ReactElement;
  readonly keyForItem: (_: A) => string;
  readonly setItems: (_: Array<A>) => void;
  readonly isItemSelected: (_: A) => boolean;
  readonly onClickItem: (_: A) => void;
};

export class dragAndDropList_ extends Component<Props<unknown>> {
  constructor(props: Props<unknown>) {
    super(props);
    this.onDragEnd = this.onDragEnd.bind(this);
  }

  onDragEnd(result: {
    destination: { index: number };
    source: { index: number };
  }) {
    // dropped outside the list
    if (!result.destination) {
      return;
    }

    const items = reorder(
      this.props.items,
      result.source.index,
      result.destination.index
    );

    this.props.setItems(items);
  }

  // Normally you would want to split things out into separate components.
  // But in this example everything is just done in one place for simplicity
  render() {
    return (
      // @ts-ignore
      <DragDropContext onDragEnd={this.onDragEnd}>
        <Droppable droppableId="droppable">
          {(provided, snapshot: { isDraggingOver: any }) => (
            <ul
              className="drag-ul"
              {...provided.droppableProps}
              ref={provided.innerRef}
              // style={getListStyle(snapshot.isDraggingOver)}
            >
              {this.props.items.map((item, index) => (
                <Draggable
                  key={this.props.keyForItem(item)}
                  draggableId={this.props.keyForItem(item)}
                  index={index}
                >
                  {(provided, snapshot: { isDragging: any }) => (
                    <li
                      ref={provided.innerRef}
                      {...provided.draggableProps}
                      {...provided.dragHandleProps}
                      style={getItemStyle(
                        snapshot.isDragging,
                        provided.draggableProps.style
                      )}
                      className={
                        this.props.isItemSelected(item) ? "selected" : ""
                      }
                      onClick={(_) => {
                        this.props.onClickItem(item);
                      }}
                    >
                      {this.props.renderItem(item)}
                    </li>
                  )}
                </Draggable>
              ))}
              {provided.placeholder}
            </ul>
          )}
        </Droppable>
      </DragDropContext>
    );
  }
}
