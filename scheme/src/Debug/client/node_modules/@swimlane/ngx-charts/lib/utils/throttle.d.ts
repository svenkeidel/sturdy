/**
 * Throttle a function
 *
 */
export declare function throttle(func: any, wait: number, options?: any): () => any;
/**
 * Throttle decorator
 *
 *  class MyClass {
 *    throttleable(10)
 *    myFn() { ... }
 *  }
 */
export declare function throttleable(duration: number, options?: any): (target: any, key: any, descriptor: any) => {
    configurable: boolean;
    enumerable: any;
    get: () => any;
};
