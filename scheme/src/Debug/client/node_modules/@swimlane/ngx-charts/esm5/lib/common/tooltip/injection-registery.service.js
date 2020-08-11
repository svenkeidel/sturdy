import { __assign } from "tslib";
var InjectionRegisteryService = /** @class */ (function () {
    function InjectionRegisteryService(injectionService) {
        this.injectionService = injectionService;
        this.defaults = {};
        this.components = new Map();
    }
    InjectionRegisteryService.prototype.getByType = function (type) {
        if (type === void 0) { type = this.type; }
        return this.components.get(type);
    };
    InjectionRegisteryService.prototype.create = function (bindings) {
        return this.createByType(this.type, bindings);
    };
    InjectionRegisteryService.prototype.createByType = function (type, bindings) {
        bindings = this.assignDefaults(bindings);
        var component = this.injectComponent(type, bindings);
        this.register(type, component);
        return component;
    };
    InjectionRegisteryService.prototype.destroy = function (instance) {
        var compsByType = this.components.get(instance.componentType);
        if (compsByType && compsByType.length) {
            var idx = compsByType.indexOf(instance);
            if (idx > -1) {
                var component = compsByType[idx];
                component.destroy();
                compsByType.splice(idx, 1);
            }
        }
    };
    InjectionRegisteryService.prototype.destroyAll = function () {
        this.destroyByType(this.type);
    };
    InjectionRegisteryService.prototype.destroyByType = function (type) {
        var comps = this.components.get(type);
        if (comps && comps.length) {
            var i = comps.length - 1;
            while (i >= 0) {
                this.destroy(comps[i--]);
            }
        }
    };
    InjectionRegisteryService.prototype.injectComponent = function (type, bindings) {
        return this.injectionService.appendComponent(type, bindings);
    };
    InjectionRegisteryService.prototype.assignDefaults = function (bindings) {
        var inputs = __assign({}, this.defaults.inputs);
        var outputs = __assign({}, this.defaults.outputs);
        if (!bindings.inputs && !bindings.outputs) {
            bindings = { inputs: bindings };
        }
        if (inputs) {
            bindings.inputs = __assign(__assign({}, inputs), bindings.inputs);
        }
        if (outputs) {
            bindings.outputs = __assign(__assign({}, outputs), bindings.outputs);
        }
        return bindings;
    };
    InjectionRegisteryService.prototype.register = function (type, component) {
        if (!this.components.has(type)) {
            this.components.set(type, []);
        }
        var types = this.components.get(type);
        types.push(component);
    };
    return InjectionRegisteryService;
}());
export { InjectionRegisteryService };
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiaW5qZWN0aW9uLXJlZ2lzdGVyeS5zZXJ2aWNlLmpzIiwic291cmNlUm9vdCI6Im5nOi8vQHN3aW1sYW5lL25neC1jaGFydHMvIiwic291cmNlcyI6WyJsaWIvY29tbW9uL3Rvb2x0aXAvaW5qZWN0aW9uLXJlZ2lzdGVyeS5zZXJ2aWNlLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7QUFRQTtJQU1FLG1DQUFtQixnQkFBa0M7UUFBbEMscUJBQWdCLEdBQWhCLGdCQUFnQixDQUFrQjtRQUgzQyxhQUFRLEdBQW9CLEVBQUUsQ0FBQztRQUMvQixlQUFVLEdBQXFDLElBQUksR0FBRyxFQUFFLENBQUM7SUFFWCxDQUFDO0lBRXpELDZDQUFTLEdBQVQsVUFBVSxJQUF5QjtRQUF6QixxQkFBQSxFQUFBLE9BQWdCLElBQUksQ0FBQyxJQUFJO1FBQ2pDLE9BQU8sSUFBSSxDQUFDLFVBQVUsQ0FBQyxHQUFHLENBQUMsSUFBSSxDQUFDLENBQUM7SUFDbkMsQ0FBQztJQUVELDBDQUFNLEdBQU4sVUFBTyxRQUFnQjtRQUNyQixPQUFPLElBQUksQ0FBQyxZQUFZLENBQUMsSUFBSSxDQUFDLElBQUksRUFBRSxRQUFRLENBQUMsQ0FBQztJQUNoRCxDQUFDO0lBRUQsZ0RBQVksR0FBWixVQUFhLElBQWEsRUFBRSxRQUF5QjtRQUNuRCxRQUFRLEdBQUcsSUFBSSxDQUFDLGNBQWMsQ0FBQyxRQUFRLENBQUMsQ0FBQztRQUV6QyxJQUFNLFNBQVMsR0FBRyxJQUFJLENBQUMsZUFBZSxDQUFDLElBQUksRUFBRSxRQUFRLENBQUMsQ0FBQztRQUN2RCxJQUFJLENBQUMsUUFBUSxDQUFDLElBQUksRUFBRSxTQUFTLENBQUMsQ0FBQztRQUUvQixPQUFPLFNBQVMsQ0FBQztJQUNuQixDQUFDO0lBRUQsMkNBQU8sR0FBUCxVQUFRLFFBQXlCO1FBQy9CLElBQU0sV0FBVyxHQUFHLElBQUksQ0FBQyxVQUFVLENBQUMsR0FBRyxDQUFDLFFBQVEsQ0FBQyxhQUFhLENBQUMsQ0FBQztRQUVoRSxJQUFJLFdBQVcsSUFBSSxXQUFXLENBQUMsTUFBTSxFQUFFO1lBQ3JDLElBQU0sR0FBRyxHQUFHLFdBQVcsQ0FBQyxPQUFPLENBQUMsUUFBUSxDQUFDLENBQUM7WUFFMUMsSUFBSSxHQUFHLEdBQUcsQ0FBQyxDQUFDLEVBQUU7Z0JBQ1osSUFBTSxTQUFTLEdBQUcsV0FBVyxDQUFDLEdBQUcsQ0FBQyxDQUFDO2dCQUNuQyxTQUFTLENBQUMsT0FBTyxFQUFFLENBQUM7Z0JBQ3BCLFdBQVcsQ0FBQyxNQUFNLENBQUMsR0FBRyxFQUFFLENBQUMsQ0FBQyxDQUFDO2FBQzVCO1NBQ0Y7SUFDSCxDQUFDO0lBRUQsOENBQVUsR0FBVjtRQUNFLElBQUksQ0FBQyxhQUFhLENBQUMsSUFBSSxDQUFDLElBQUksQ0FBQyxDQUFDO0lBQ2hDLENBQUM7SUFFRCxpREFBYSxHQUFiLFVBQWMsSUFBYTtRQUN6QixJQUFNLEtBQUssR0FBRyxJQUFJLENBQUMsVUFBVSxDQUFDLEdBQUcsQ0FBQyxJQUFJLENBQUMsQ0FBQztRQUV4QyxJQUFJLEtBQUssSUFBSSxLQUFLLENBQUMsTUFBTSxFQUFFO1lBQ3pCLElBQUksQ0FBQyxHQUFHLEtBQUssQ0FBQyxNQUFNLEdBQUcsQ0FBQyxDQUFDO1lBQ3pCLE9BQU8sQ0FBQyxJQUFJLENBQUMsRUFBRTtnQkFDYixJQUFJLENBQUMsT0FBTyxDQUFDLEtBQUssQ0FBQyxDQUFDLEVBQUUsQ0FBQyxDQUFDLENBQUM7YUFDMUI7U0FDRjtJQUNILENBQUM7SUFFUyxtREFBZSxHQUF6QixVQUEwQixJQUFhLEVBQUUsUUFBeUI7UUFDaEUsT0FBTyxJQUFJLENBQUMsZ0JBQWdCLENBQUMsZUFBZSxDQUFDLElBQUksRUFBRSxRQUFRLENBQUMsQ0FBQztJQUMvRCxDQUFDO0lBRVMsa0RBQWMsR0FBeEIsVUFBeUIsUUFBeUI7UUFDaEQsSUFBTSxNQUFNLGdCQUFRLElBQUksQ0FBQyxRQUFRLENBQUMsTUFBTSxDQUFFLENBQUM7UUFDM0MsSUFBTSxPQUFPLGdCQUFRLElBQUksQ0FBQyxRQUFRLENBQUMsT0FBTyxDQUFFLENBQUM7UUFFN0MsSUFBSSxDQUFDLFFBQVEsQ0FBQyxNQUFNLElBQUksQ0FBQyxRQUFRLENBQUMsT0FBTyxFQUFFO1lBQ3pDLFFBQVEsR0FBRyxFQUFFLE1BQU0sRUFBRSxRQUFRLEVBQUUsQ0FBQztTQUNqQztRQUVELElBQUksTUFBTSxFQUFFO1lBQ1YsUUFBUSxDQUFDLE1BQU0seUJBQVEsTUFBTSxHQUFLLFFBQVEsQ0FBQyxNQUFNLENBQUUsQ0FBQztTQUNyRDtRQUVELElBQUksT0FBTyxFQUFFO1lBQ1gsUUFBUSxDQUFDLE9BQU8seUJBQVEsT0FBTyxHQUFLLFFBQVEsQ0FBQyxPQUFPLENBQUUsQ0FBQztTQUN4RDtRQUVELE9BQU8sUUFBUSxDQUFDO0lBQ2xCLENBQUM7SUFFUyw0Q0FBUSxHQUFsQixVQUFtQixJQUFhLEVBQUUsU0FBMEI7UUFDMUQsSUFBSSxDQUFDLElBQUksQ0FBQyxVQUFVLENBQUMsR0FBRyxDQUFDLElBQUksQ0FBQyxFQUFFO1lBQzlCLElBQUksQ0FBQyxVQUFVLENBQUMsR0FBRyxDQUFDLElBQUksRUFBRSxFQUFFLENBQUMsQ0FBQztTQUMvQjtRQUVELElBQU0sS0FBSyxHQUFHLElBQUksQ0FBQyxVQUFVLENBQUMsR0FBRyxDQUFDLElBQUksQ0FBQyxDQUFDO1FBQ3hDLEtBQUssQ0FBQyxJQUFJLENBQUMsU0FBUyxDQUFDLENBQUM7SUFDeEIsQ0FBQztJQUNILGdDQUFDO0FBQUQsQ0FBQyxBQXJGRCxJQXFGQyIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCB7IENvbXBvbmVudFJlZiwgVHlwZSB9IGZyb20gJ0Bhbmd1bGFyL2NvcmUnO1xuaW1wb3J0IHsgSW5qZWN0aW9uU2VydmljZSB9IGZyb20gJy4vaW5qZWN0aW9uLnNlcnZpY2UnO1xuXG5leHBvcnQgaW50ZXJmYWNlIFBhcnRpYWxCaW5kaW5ncyB7XG4gIGlucHV0cz86IG9iamVjdDtcbiAgb3V0cHV0cz86IG9iamVjdDtcbn1cblxuZXhwb3J0IGFic3RyYWN0IGNsYXNzIEluamVjdGlvblJlZ2lzdGVyeVNlcnZpY2U8VCA9IGFueT4ge1xuICBwcm90ZWN0ZWQgYWJzdHJhY3QgdHlwZTogVHlwZTxUPjtcblxuICBwcm90ZWN0ZWQgZGVmYXVsdHM6IFBhcnRpYWxCaW5kaW5ncyA9IHt9O1xuICBwcm90ZWN0ZWQgY29tcG9uZW50czogTWFwPGFueSwgQXJyYXk8Q29tcG9uZW50UmVmPFQ+Pj4gPSBuZXcgTWFwKCk7XG5cbiAgY29uc3RydWN0b3IocHVibGljIGluamVjdGlvblNlcnZpY2U6IEluamVjdGlvblNlcnZpY2UpIHt9XG5cbiAgZ2V0QnlUeXBlKHR5cGU6IFR5cGU8VD4gPSB0aGlzLnR5cGUpIHtcbiAgICByZXR1cm4gdGhpcy5jb21wb25lbnRzLmdldCh0eXBlKTtcbiAgfVxuXG4gIGNyZWF0ZShiaW5kaW5nczogb2JqZWN0KTogQ29tcG9uZW50UmVmPFQ+IHtcbiAgICByZXR1cm4gdGhpcy5jcmVhdGVCeVR5cGUodGhpcy50eXBlLCBiaW5kaW5ncyk7XG4gIH1cblxuICBjcmVhdGVCeVR5cGUodHlwZTogVHlwZTxUPiwgYmluZGluZ3M6IFBhcnRpYWxCaW5kaW5ncyk6IENvbXBvbmVudFJlZjxUPiB7XG4gICAgYmluZGluZ3MgPSB0aGlzLmFzc2lnbkRlZmF1bHRzKGJpbmRpbmdzKTtcblxuICAgIGNvbnN0IGNvbXBvbmVudCA9IHRoaXMuaW5qZWN0Q29tcG9uZW50KHR5cGUsIGJpbmRpbmdzKTtcbiAgICB0aGlzLnJlZ2lzdGVyKHR5cGUsIGNvbXBvbmVudCk7XG5cbiAgICByZXR1cm4gY29tcG9uZW50O1xuICB9XG5cbiAgZGVzdHJveShpbnN0YW5jZTogQ29tcG9uZW50UmVmPFQ+KTogdm9pZCB7XG4gICAgY29uc3QgY29tcHNCeVR5cGUgPSB0aGlzLmNvbXBvbmVudHMuZ2V0KGluc3RhbmNlLmNvbXBvbmVudFR5cGUpO1xuXG4gICAgaWYgKGNvbXBzQnlUeXBlICYmIGNvbXBzQnlUeXBlLmxlbmd0aCkge1xuICAgICAgY29uc3QgaWR4ID0gY29tcHNCeVR5cGUuaW5kZXhPZihpbnN0YW5jZSk7XG5cbiAgICAgIGlmIChpZHggPiAtMSkge1xuICAgICAgICBjb25zdCBjb21wb25lbnQgPSBjb21wc0J5VHlwZVtpZHhdO1xuICAgICAgICBjb21wb25lbnQuZGVzdHJveSgpO1xuICAgICAgICBjb21wc0J5VHlwZS5zcGxpY2UoaWR4LCAxKTtcbiAgICAgIH1cbiAgICB9XG4gIH1cblxuICBkZXN0cm95QWxsKCk6IHZvaWQge1xuICAgIHRoaXMuZGVzdHJveUJ5VHlwZSh0aGlzLnR5cGUpO1xuICB9XG5cbiAgZGVzdHJveUJ5VHlwZSh0eXBlOiBUeXBlPFQ+KTogdm9pZCB7XG4gICAgY29uc3QgY29tcHMgPSB0aGlzLmNvbXBvbmVudHMuZ2V0KHR5cGUpO1xuXG4gICAgaWYgKGNvbXBzICYmIGNvbXBzLmxlbmd0aCkge1xuICAgICAgbGV0IGkgPSBjb21wcy5sZW5ndGggLSAxO1xuICAgICAgd2hpbGUgKGkgPj0gMCkge1xuICAgICAgICB0aGlzLmRlc3Ryb3koY29tcHNbaS0tXSk7XG4gICAgICB9XG4gICAgfVxuICB9XG5cbiAgcHJvdGVjdGVkIGluamVjdENvbXBvbmVudCh0eXBlOiBUeXBlPFQ+LCBiaW5kaW5nczogUGFydGlhbEJpbmRpbmdzKTogQ29tcG9uZW50UmVmPFQ+IHtcbiAgICByZXR1cm4gdGhpcy5pbmplY3Rpb25TZXJ2aWNlLmFwcGVuZENvbXBvbmVudCh0eXBlLCBiaW5kaW5ncyk7XG4gIH1cblxuICBwcm90ZWN0ZWQgYXNzaWduRGVmYXVsdHMoYmluZGluZ3M6IFBhcnRpYWxCaW5kaW5ncyk6IFBhcnRpYWxCaW5kaW5ncyB7XG4gICAgY29uc3QgaW5wdXRzID0geyAuLi50aGlzLmRlZmF1bHRzLmlucHV0cyB9O1xuICAgIGNvbnN0IG91dHB1dHMgPSB7IC4uLnRoaXMuZGVmYXVsdHMub3V0cHV0cyB9O1xuXG4gICAgaWYgKCFiaW5kaW5ncy5pbnB1dHMgJiYgIWJpbmRpbmdzLm91dHB1dHMpIHtcbiAgICAgIGJpbmRpbmdzID0geyBpbnB1dHM6IGJpbmRpbmdzIH07XG4gICAgfVxuXG4gICAgaWYgKGlucHV0cykge1xuICAgICAgYmluZGluZ3MuaW5wdXRzID0geyAuLi5pbnB1dHMsIC4uLmJpbmRpbmdzLmlucHV0cyB9O1xuICAgIH1cblxuICAgIGlmIChvdXRwdXRzKSB7XG4gICAgICBiaW5kaW5ncy5vdXRwdXRzID0geyAuLi5vdXRwdXRzLCAuLi5iaW5kaW5ncy5vdXRwdXRzIH07XG4gICAgfVxuXG4gICAgcmV0dXJuIGJpbmRpbmdzO1xuICB9XG5cbiAgcHJvdGVjdGVkIHJlZ2lzdGVyKHR5cGU6IFR5cGU8VD4sIGNvbXBvbmVudDogQ29tcG9uZW50UmVmPFQ+KTogdm9pZCB7XG4gICAgaWYgKCF0aGlzLmNvbXBvbmVudHMuaGFzKHR5cGUpKSB7XG4gICAgICB0aGlzLmNvbXBvbmVudHMuc2V0KHR5cGUsIFtdKTtcbiAgICB9XG5cbiAgICBjb25zdCB0eXBlcyA9IHRoaXMuY29tcG9uZW50cy5nZXQodHlwZSk7XG4gICAgdHlwZXMucHVzaChjb21wb25lbnQpO1xuICB9XG59XG4iXX0=