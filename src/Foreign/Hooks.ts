import { RefObject, useEffect, useState } from "react";

// export function useIntersectionObserver_(
//   elementRef: RefObject<Element>,
//   withIntersectionObserver: (isIntersecting: boolean) => () => void
// ): void {
//   const [isIntersecting, setIsIntersecting] = useState(false);

//   const node = elementRef?.current; // DOM Ref

//   useEffect(() => {
//     console.log("1 >>>>", node);

//     if (!node) return;
//     console.log("DIRT >>>>", node);

//     const observer = new IntersectionObserver(
//       ([entry]: IntersectionObserverEntry[]): void => {
//         console.log({ entry });
//         setIsIntersecting(entry?.isIntersecting ?? false);
//       }
//     );

//     observer.observe(node);

//     return () => observer.disconnect();
//   }, [node]);

//   // console.log(elementRef);
//   // console.log("this dirt", isIntersecting);

//   useEffect(withIntersectionObserver(isIntersecting), [isIntersecting]);

//   return;
// }

/**********/
/**********/
/**********/
/**********/
/**********/
/**********/

interface Args extends IntersectionObserverInit {
  freezeOnceVisible?: boolean;
}

export function useIntersectionObserver_(
  elementRef: RefObject<Element>
): boolean {
  const [entry, setEntry] = useState<IntersectionObserverEntry>();

  // const frozen = entry?.isIntersecting && freezeOnceVisible;

  const updateEntry = ([entry]: IntersectionObserverEntry[]): void => {
    setEntry(entry);
  };

  useEffect(() => {
    const node = elementRef?.current; // DOM Ref
    const hasIOSupport = !!window.IntersectionObserver;

    if (!hasIOSupport || !node) return;

    const observerParams = {
      threshold: 0,
      root: null,
      rootMargin: "0%",
    };
    const observer = new IntersectionObserver(updateEntry, observerParams);

    observer.observe(node);

    return () => observer.disconnect();
  }, [elementRef?.current]);

  return entry?.isIntersecting ?? false;
}
