import { useDebounce } from "use-debounce";

export const useDebounce_ = <T>(value: T, delay: number) =>
  useDebounce(value, delay)[0];
