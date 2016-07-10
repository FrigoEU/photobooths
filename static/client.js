(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
(function (global){
var topLevel = typeof global !== 'undefined' ? global :
    typeof window !== 'undefined' ? window : {}
var minDoc = require('min-document');

if (typeof document !== 'undefined') {
    module.exports = document;
} else {
    var doccy = topLevel['__GLOBAL_DOCUMENT_CACHE@4'];

    if (!doccy) {
        doccy = topLevel['__GLOBAL_DOCUMENT_CACHE@4'] = minDoc;
    }

    module.exports = doccy;
}

}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"min-document":27}],2:[function(require,module,exports){
"use strict";

module.exports = function isObject(x) {
	return typeof x === "object" && x !== null;
};

},{}],3:[function(require,module,exports){
var createElement = require("./vdom/create-element.js")

module.exports = createElement

},{"./vdom/create-element.js":7}],4:[function(require,module,exports){
var diff = require("./vtree/diff.js")

module.exports = diff

},{"./vtree/diff.js":24}],5:[function(require,module,exports){
var patch = require("./vdom/patch.js")

module.exports = patch

},{"./vdom/patch.js":10}],6:[function(require,module,exports){
var isObject = require("is-object")
var isHook = require("../vnode/is-vhook.js")

module.exports = applyProperties

function applyProperties(node, props, previous) {
    for (var propName in props) {
        var propValue = props[propName]

        if (propValue === undefined) {
            removeProperty(node, propName, propValue, previous);
        } else if (isHook(propValue)) {
            removeProperty(node, propName, propValue, previous)
            if (propValue.hook) {
                propValue.hook(node,
                    propName,
                    previous ? previous[propName] : undefined)
            }
        } else {
            if (isObject(propValue)) {
                patchObject(node, props, previous, propName, propValue);
            } else {
                node[propName] = propValue
            }
        }
    }
}

function removeProperty(node, propName, propValue, previous) {
    if (previous) {
        var previousValue = previous[propName]

        if (!isHook(previousValue)) {
            if (propName === "attributes") {
                for (var attrName in previousValue) {
                    node.removeAttribute(attrName)
                }
            } else if (propName === "style") {
                for (var i in previousValue) {
                    node.style[i] = ""
                }
            } else if (typeof previousValue === "string") {
                node[propName] = ""
            } else {
                node[propName] = null
            }
        } else if (previousValue.unhook) {
            previousValue.unhook(node, propName, propValue)
        }
    }
}

function patchObject(node, props, previous, propName, propValue) {
    var previousValue = previous ? previous[propName] : undefined

    // Set attributes
    if (propName === "attributes") {
        for (var attrName in propValue) {
            var attrValue = propValue[attrName]

            if (attrValue === undefined) {
                node.removeAttribute(attrName)
            } else {
                node.setAttribute(attrName, attrValue)
            }
        }

        return
    }

    if(previousValue && isObject(previousValue) &&
        getPrototype(previousValue) !== getPrototype(propValue)) {
        node[propName] = propValue
        return
    }

    if (!isObject(node[propName])) {
        node[propName] = {}
    }

    var replacer = propName === "style" ? "" : undefined

    for (var k in propValue) {
        var value = propValue[k]
        node[propName][k] = (value === undefined) ? replacer : value
    }
}

function getPrototype(value) {
    if (Object.getPrototypeOf) {
        return Object.getPrototypeOf(value)
    } else if (value.__proto__) {
        return value.__proto__
    } else if (value.constructor) {
        return value.constructor.prototype
    }
}

},{"../vnode/is-vhook.js":15,"is-object":2}],7:[function(require,module,exports){
var document = require("global/document")

var applyProperties = require("./apply-properties")

var isVNode = require("../vnode/is-vnode.js")
var isVText = require("../vnode/is-vtext.js")
var isWidget = require("../vnode/is-widget.js")
var handleThunk = require("../vnode/handle-thunk.js")

module.exports = createElement

function createElement(vnode, opts) {
    var doc = opts ? opts.document || document : document
    var warn = opts ? opts.warn : null

    vnode = handleThunk(vnode).a

    if (isWidget(vnode)) {
        return vnode.init()
    } else if (isVText(vnode)) {
        return doc.createTextNode(vnode.text)
    } else if (!isVNode(vnode)) {
        if (warn) {
            warn("Item is not a valid virtual dom node", vnode)
        }
        return null
    }

    var node = (vnode.namespace === null) ?
        doc.createElement(vnode.tagName) :
        doc.createElementNS(vnode.namespace, vnode.tagName)

    var props = vnode.properties
    applyProperties(node, props)

    var children = vnode.children

    for (var i = 0; i < children.length; i++) {
        var childNode = createElement(children[i], opts)
        if (childNode) {
            node.appendChild(childNode)
        }
    }

    return node
}

},{"../vnode/handle-thunk.js":13,"../vnode/is-vnode.js":16,"../vnode/is-vtext.js":17,"../vnode/is-widget.js":18,"./apply-properties":6,"global/document":1}],8:[function(require,module,exports){
// Maps a virtual DOM tree onto a real DOM tree in an efficient manner.
// We don't want to read all of the DOM nodes in the tree so we use
// the in-order tree indexing to eliminate recursion down certain branches.
// We only recurse into a DOM node if we know that it contains a child of
// interest.

var noChild = {}

module.exports = domIndex

function domIndex(rootNode, tree, indices, nodes) {
    if (!indices || indices.length === 0) {
        return {}
    } else {
        indices.sort(ascending)
        return recurse(rootNode, tree, indices, nodes, 0)
    }
}

function recurse(rootNode, tree, indices, nodes, rootIndex) {
    nodes = nodes || {}


    if (rootNode) {
        if (indexInRange(indices, rootIndex, rootIndex)) {
            nodes[rootIndex] = rootNode
        }

        var vChildren = tree.children

        if (vChildren) {

            var childNodes = rootNode.childNodes

            for (var i = 0; i < tree.children.length; i++) {
                rootIndex += 1

                var vChild = vChildren[i] || noChild
                var nextIndex = rootIndex + (vChild.count || 0)

                // skip recursion down the tree if there are no nodes down here
                if (indexInRange(indices, rootIndex, nextIndex)) {
                    recurse(childNodes[i], vChild, indices, nodes, rootIndex)
                }

                rootIndex = nextIndex
            }
        }
    }

    return nodes
}

// Binary search for an index in the interval [left, right]
function indexInRange(indices, left, right) {
    if (indices.length === 0) {
        return false
    }

    var minIndex = 0
    var maxIndex = indices.length - 1
    var currentIndex
    var currentItem

    while (minIndex <= maxIndex) {
        currentIndex = ((maxIndex + minIndex) / 2) >> 0
        currentItem = indices[currentIndex]

        if (minIndex === maxIndex) {
            return currentItem >= left && currentItem <= right
        } else if (currentItem < left) {
            minIndex = currentIndex + 1
        } else  if (currentItem > right) {
            maxIndex = currentIndex - 1
        } else {
            return true
        }
    }

    return false;
}

function ascending(a, b) {
    return a > b ? 1 : -1
}

},{}],9:[function(require,module,exports){
var applyProperties = require("./apply-properties")

var isWidget = require("../vnode/is-widget.js")
var VPatch = require("../vnode/vpatch.js")

var updateWidget = require("./update-widget")

module.exports = applyPatch

function applyPatch(vpatch, domNode, renderOptions) {
    var type = vpatch.type
    var vNode = vpatch.vNode
    var patch = vpatch.patch

    switch (type) {
        case VPatch.REMOVE:
            return removeNode(domNode, vNode)
        case VPatch.INSERT:
            return insertNode(domNode, patch, renderOptions)
        case VPatch.VTEXT:
            return stringPatch(domNode, vNode, patch, renderOptions)
        case VPatch.WIDGET:
            return widgetPatch(domNode, vNode, patch, renderOptions)
        case VPatch.VNODE:
            return vNodePatch(domNode, vNode, patch, renderOptions)
        case VPatch.ORDER:
            reorderChildren(domNode, patch)
            return domNode
        case VPatch.PROPS:
            applyProperties(domNode, patch, vNode.properties)
            return domNode
        case VPatch.THUNK:
            return replaceRoot(domNode,
                renderOptions.patch(domNode, patch, renderOptions))
        default:
            return domNode
    }
}

function removeNode(domNode, vNode) {
    var parentNode = domNode.parentNode

    if (parentNode) {
        parentNode.removeChild(domNode)
    }

    destroyWidget(domNode, vNode);

    return null
}

function insertNode(parentNode, vNode, renderOptions) {
    var newNode = renderOptions.render(vNode, renderOptions)

    if (parentNode) {
        parentNode.appendChild(newNode)
    }

    return parentNode
}

function stringPatch(domNode, leftVNode, vText, renderOptions) {
    var newNode

    if (domNode.nodeType === 3) {
        domNode.replaceData(0, domNode.length, vText.text)
        newNode = domNode
    } else {
        var parentNode = domNode.parentNode
        newNode = renderOptions.render(vText, renderOptions)

        if (parentNode && newNode !== domNode) {
            parentNode.replaceChild(newNode, domNode)
        }
    }

    return newNode
}

function widgetPatch(domNode, leftVNode, widget, renderOptions) {
    var updating = updateWidget(leftVNode, widget)
    var newNode

    if (updating) {
        newNode = widget.update(leftVNode, domNode) || domNode
    } else {
        newNode = renderOptions.render(widget, renderOptions)
    }

    var parentNode = domNode.parentNode

    if (parentNode && newNode !== domNode) {
        parentNode.replaceChild(newNode, domNode)
    }

    if (!updating) {
        destroyWidget(domNode, leftVNode)
    }

    return newNode
}

function vNodePatch(domNode, leftVNode, vNode, renderOptions) {
    var parentNode = domNode.parentNode
    var newNode = renderOptions.render(vNode, renderOptions)

    if (parentNode && newNode !== domNode) {
        parentNode.replaceChild(newNode, domNode)
    }

    return newNode
}

function destroyWidget(domNode, w) {
    if (typeof w.destroy === "function" && isWidget(w)) {
        w.destroy(domNode)
    }
}

function reorderChildren(domNode, moves) {
    var childNodes = domNode.childNodes
    var keyMap = {}
    var node
    var remove
    var insert

    for (var i = 0; i < moves.removes.length; i++) {
        remove = moves.removes[i]
        node = childNodes[remove.from]
        if (remove.key) {
            keyMap[remove.key] = node
        }
        domNode.removeChild(node)
    }

    var length = childNodes.length
    for (var j = 0; j < moves.inserts.length; j++) {
        insert = moves.inserts[j]
        node = keyMap[insert.key]
        // this is the weirdest bug i've ever seen in webkit
        domNode.insertBefore(node, insert.to >= length++ ? null : childNodes[insert.to])
    }
}

function replaceRoot(oldRoot, newRoot) {
    if (oldRoot && newRoot && oldRoot !== newRoot && oldRoot.parentNode) {
        oldRoot.parentNode.replaceChild(newRoot, oldRoot)
    }

    return newRoot;
}

},{"../vnode/is-widget.js":18,"../vnode/vpatch.js":21,"./apply-properties":6,"./update-widget":11}],10:[function(require,module,exports){
var document = require("global/document")
var isArray = require("x-is-array")

var render = require("./create-element")
var domIndex = require("./dom-index")
var patchOp = require("./patch-op")
module.exports = patch

function patch(rootNode, patches, renderOptions) {
    renderOptions = renderOptions || {}
    renderOptions.patch = renderOptions.patch && renderOptions.patch !== patch
        ? renderOptions.patch
        : patchRecursive
    renderOptions.render = renderOptions.render || render

    return renderOptions.patch(rootNode, patches, renderOptions)
}

function patchRecursive(rootNode, patches, renderOptions) {
    var indices = patchIndices(patches)

    if (indices.length === 0) {
        return rootNode
    }

    var index = domIndex(rootNode, patches.a, indices)
    var ownerDocument = rootNode.ownerDocument

    if (!renderOptions.document && ownerDocument !== document) {
        renderOptions.document = ownerDocument
    }

    for (var i = 0; i < indices.length; i++) {
        var nodeIndex = indices[i]
        rootNode = applyPatch(rootNode,
            index[nodeIndex],
            patches[nodeIndex],
            renderOptions)
    }

    return rootNode
}

function applyPatch(rootNode, domNode, patchList, renderOptions) {
    if (!domNode) {
        return rootNode
    }

    var newNode

    if (isArray(patchList)) {
        for (var i = 0; i < patchList.length; i++) {
            newNode = patchOp(patchList[i], domNode, renderOptions)

            if (domNode === rootNode) {
                rootNode = newNode
            }
        }
    } else {
        newNode = patchOp(patchList, domNode, renderOptions)

        if (domNode === rootNode) {
            rootNode = newNode
        }
    }

    return rootNode
}

function patchIndices(patches) {
    var indices = []

    for (var key in patches) {
        if (key !== "a") {
            indices.push(Number(key))
        }
    }

    return indices
}

},{"./create-element":7,"./dom-index":8,"./patch-op":9,"global/document":1,"x-is-array":25}],11:[function(require,module,exports){
var isWidget = require("../vnode/is-widget.js")

module.exports = updateWidget

function updateWidget(a, b) {
    if (isWidget(a) && isWidget(b)) {
        if ("name" in a && "name" in b) {
            return a.id === b.id
        } else {
            return a.init === b.init
        }
    }

    return false
}

},{"../vnode/is-widget.js":18}],12:[function(require,module,exports){
'use strict';

module.exports = SoftSetHook;

function SoftSetHook(value) {
    if (!(this instanceof SoftSetHook)) {
        return new SoftSetHook(value);
    }

    this.value = value;
}

SoftSetHook.prototype.hook = function (node, propertyName) {
    if (node[propertyName] !== this.value) {
        node[propertyName] = this.value;
    }
};

},{}],13:[function(require,module,exports){
var isVNode = require("./is-vnode")
var isVText = require("./is-vtext")
var isWidget = require("./is-widget")
var isThunk = require("./is-thunk")

module.exports = handleThunk

function handleThunk(a, b) {
    var renderedA = a
    var renderedB = b

    if (isThunk(b)) {
        renderedB = renderThunk(b, a)
    }

    if (isThunk(a)) {
        renderedA = renderThunk(a, null)
    }

    return {
        a: renderedA,
        b: renderedB
    }
}

function renderThunk(thunk, previous) {
    var renderedThunk = thunk.vnode

    if (!renderedThunk) {
        renderedThunk = thunk.vnode = thunk.render(previous)
    }

    if (!(isVNode(renderedThunk) ||
            isVText(renderedThunk) ||
            isWidget(renderedThunk))) {
        throw new Error("thunk did not return a valid node");
    }

    return renderedThunk
}

},{"./is-thunk":14,"./is-vnode":16,"./is-vtext":17,"./is-widget":18}],14:[function(require,module,exports){
module.exports = isThunk

function isThunk(t) {
    return t && t.type === "Thunk"
}

},{}],15:[function(require,module,exports){
module.exports = isHook

function isHook(hook) {
    return hook &&
      (typeof hook.hook === "function" && !hook.hasOwnProperty("hook") ||
       typeof hook.unhook === "function" && !hook.hasOwnProperty("unhook"))
}

},{}],16:[function(require,module,exports){
var version = require("./version")

module.exports = isVirtualNode

function isVirtualNode(x) {
    return x && x.type === "VirtualNode" && x.version === version
}

},{"./version":19}],17:[function(require,module,exports){
var version = require("./version")

module.exports = isVirtualText

function isVirtualText(x) {
    return x && x.type === "VirtualText" && x.version === version
}

},{"./version":19}],18:[function(require,module,exports){
module.exports = isWidget

function isWidget(w) {
    return w && w.type === "Widget"
}

},{}],19:[function(require,module,exports){
module.exports = "2"

},{}],20:[function(require,module,exports){
var version = require("./version")
var isVNode = require("./is-vnode")
var isWidget = require("./is-widget")
var isThunk = require("./is-thunk")
var isVHook = require("./is-vhook")

module.exports = VirtualNode

var noProperties = {}
var noChildren = []

function VirtualNode(tagName, properties, children, key, namespace) {
    this.tagName = tagName
    this.properties = properties || noProperties
    this.children = children || noChildren
    this.key = key != null ? String(key) : undefined
    this.namespace = (typeof namespace === "string") ? namespace : null

    var count = (children && children.length) || 0
    var descendants = 0
    var hasWidgets = false
    var hasThunks = false
    var descendantHooks = false
    var hooks

    for (var propName in properties) {
        if (properties.hasOwnProperty(propName)) {
            var property = properties[propName]
            if (isVHook(property) && property.unhook) {
                if (!hooks) {
                    hooks = {}
                }

                hooks[propName] = property
            }
        }
    }

    for (var i = 0; i < count; i++) {
        var child = children[i]
        if (isVNode(child)) {
            descendants += child.count || 0

            if (!hasWidgets && child.hasWidgets) {
                hasWidgets = true
            }

            if (!hasThunks && child.hasThunks) {
                hasThunks = true
            }

            if (!descendantHooks && (child.hooks || child.descendantHooks)) {
                descendantHooks = true
            }
        } else if (!hasWidgets && isWidget(child)) {
            if (typeof child.destroy === "function") {
                hasWidgets = true
            }
        } else if (!hasThunks && isThunk(child)) {
            hasThunks = true;
        }
    }

    this.count = count + descendants
    this.hasWidgets = hasWidgets
    this.hasThunks = hasThunks
    this.hooks = hooks
    this.descendantHooks = descendantHooks
}

VirtualNode.prototype.version = version
VirtualNode.prototype.type = "VirtualNode"

},{"./is-thunk":14,"./is-vhook":15,"./is-vnode":16,"./is-widget":18,"./version":19}],21:[function(require,module,exports){
var version = require("./version")

VirtualPatch.NONE = 0
VirtualPatch.VTEXT = 1
VirtualPatch.VNODE = 2
VirtualPatch.WIDGET = 3
VirtualPatch.PROPS = 4
VirtualPatch.ORDER = 5
VirtualPatch.INSERT = 6
VirtualPatch.REMOVE = 7
VirtualPatch.THUNK = 8

module.exports = VirtualPatch

function VirtualPatch(type, vNode, patch) {
    this.type = Number(type)
    this.vNode = vNode
    this.patch = patch
}

VirtualPatch.prototype.version = version
VirtualPatch.prototype.type = "VirtualPatch"

},{"./version":19}],22:[function(require,module,exports){
var version = require("./version")

module.exports = VirtualText

function VirtualText(text) {
    this.text = String(text)
}

VirtualText.prototype.version = version
VirtualText.prototype.type = "VirtualText"

},{"./version":19}],23:[function(require,module,exports){
var isObject = require("is-object")
var isHook = require("../vnode/is-vhook")

module.exports = diffProps

function diffProps(a, b) {
    var diff

    for (var aKey in a) {
        if (!(aKey in b)) {
            diff = diff || {}
            diff[aKey] = undefined
        }

        var aValue = a[aKey]
        var bValue = b[aKey]

        if (aValue === bValue) {
            continue
        } else if (isObject(aValue) && isObject(bValue)) {
            if (getPrototype(bValue) !== getPrototype(aValue)) {
                diff = diff || {}
                diff[aKey] = bValue
            } else if (isHook(bValue)) {
                 diff = diff || {}
                 diff[aKey] = bValue
            } else {
                var objectDiff = diffProps(aValue, bValue)
                if (objectDiff) {
                    diff = diff || {}
                    diff[aKey] = objectDiff
                }
            }
        } else {
            diff = diff || {}
            diff[aKey] = bValue
        }
    }

    for (var bKey in b) {
        if (!(bKey in a)) {
            diff = diff || {}
            diff[bKey] = b[bKey]
        }
    }

    return diff
}

function getPrototype(value) {
  if (Object.getPrototypeOf) {
    return Object.getPrototypeOf(value)
  } else if (value.__proto__) {
    return value.__proto__
  } else if (value.constructor) {
    return value.constructor.prototype
  }
}

},{"../vnode/is-vhook":15,"is-object":2}],24:[function(require,module,exports){
var isArray = require("x-is-array")

var VPatch = require("../vnode/vpatch")
var isVNode = require("../vnode/is-vnode")
var isVText = require("../vnode/is-vtext")
var isWidget = require("../vnode/is-widget")
var isThunk = require("../vnode/is-thunk")
var handleThunk = require("../vnode/handle-thunk")

var diffProps = require("./diff-props")

module.exports = diff

function diff(a, b) {
    var patch = { a: a }
    walk(a, b, patch, 0)
    return patch
}

function walk(a, b, patch, index) {
    if (a === b) {
        return
    }

    var apply = patch[index]
    var applyClear = false

    if (isThunk(a) || isThunk(b)) {
        thunks(a, b, patch, index)
    } else if (b == null) {

        // If a is a widget we will add a remove patch for it
        // Otherwise any child widgets/hooks must be destroyed.
        // This prevents adding two remove patches for a widget.
        if (!isWidget(a)) {
            clearState(a, patch, index)
            apply = patch[index]
        }

        apply = appendPatch(apply, new VPatch(VPatch.REMOVE, a, b))
    } else if (isVNode(b)) {
        if (isVNode(a)) {
            if (a.tagName === b.tagName &&
                a.namespace === b.namespace &&
                a.key === b.key) {
                var propsPatch = diffProps(a.properties, b.properties)
                if (propsPatch) {
                    apply = appendPatch(apply,
                        new VPatch(VPatch.PROPS, a, propsPatch))
                }
                apply = diffChildren(a, b, patch, apply, index)
            } else {
                apply = appendPatch(apply, new VPatch(VPatch.VNODE, a, b))
                applyClear = true
            }
        } else {
            apply = appendPatch(apply, new VPatch(VPatch.VNODE, a, b))
            applyClear = true
        }
    } else if (isVText(b)) {
        if (!isVText(a)) {
            apply = appendPatch(apply, new VPatch(VPatch.VTEXT, a, b))
            applyClear = true
        } else if (a.text !== b.text) {
            apply = appendPatch(apply, new VPatch(VPatch.VTEXT, a, b))
        }
    } else if (isWidget(b)) {
        if (!isWidget(a)) {
            applyClear = true
        }

        apply = appendPatch(apply, new VPatch(VPatch.WIDGET, a, b))
    }

    if (apply) {
        patch[index] = apply
    }

    if (applyClear) {
        clearState(a, patch, index)
    }
}

function diffChildren(a, b, patch, apply, index) {
    var aChildren = a.children
    var orderedSet = reorder(aChildren, b.children)
    var bChildren = orderedSet.children

    var aLen = aChildren.length
    var bLen = bChildren.length
    var len = aLen > bLen ? aLen : bLen

    for (var i = 0; i < len; i++) {
        var leftNode = aChildren[i]
        var rightNode = bChildren[i]
        index += 1

        if (!leftNode) {
            if (rightNode) {
                // Excess nodes in b need to be added
                apply = appendPatch(apply,
                    new VPatch(VPatch.INSERT, null, rightNode))
            }
        } else {
            walk(leftNode, rightNode, patch, index)
        }

        if (isVNode(leftNode) && leftNode.count) {
            index += leftNode.count
        }
    }

    if (orderedSet.moves) {
        // Reorder nodes last
        apply = appendPatch(apply, new VPatch(
            VPatch.ORDER,
            a,
            orderedSet.moves
        ))
    }

    return apply
}

function clearState(vNode, patch, index) {
    // TODO: Make this a single walk, not two
    unhook(vNode, patch, index)
    destroyWidgets(vNode, patch, index)
}

// Patch records for all destroyed widgets must be added because we need
// a DOM node reference for the destroy function
function destroyWidgets(vNode, patch, index) {
    if (isWidget(vNode)) {
        if (typeof vNode.destroy === "function") {
            patch[index] = appendPatch(
                patch[index],
                new VPatch(VPatch.REMOVE, vNode, null)
            )
        }
    } else if (isVNode(vNode) && (vNode.hasWidgets || vNode.hasThunks)) {
        var children = vNode.children
        var len = children.length
        for (var i = 0; i < len; i++) {
            var child = children[i]
            index += 1

            destroyWidgets(child, patch, index)

            if (isVNode(child) && child.count) {
                index += child.count
            }
        }
    } else if (isThunk(vNode)) {
        thunks(vNode, null, patch, index)
    }
}

// Create a sub-patch for thunks
function thunks(a, b, patch, index) {
    var nodes = handleThunk(a, b)
    var thunkPatch = diff(nodes.a, nodes.b)
    if (hasPatches(thunkPatch)) {
        patch[index] = new VPatch(VPatch.THUNK, null, thunkPatch)
    }
}

function hasPatches(patch) {
    for (var index in patch) {
        if (index !== "a") {
            return true
        }
    }

    return false
}

// Execute hooks when two nodes are identical
function unhook(vNode, patch, index) {
    if (isVNode(vNode)) {
        if (vNode.hooks) {
            patch[index] = appendPatch(
                patch[index],
                new VPatch(
                    VPatch.PROPS,
                    vNode,
                    undefinedKeys(vNode.hooks)
                )
            )
        }

        if (vNode.descendantHooks || vNode.hasThunks) {
            var children = vNode.children
            var len = children.length
            for (var i = 0; i < len; i++) {
                var child = children[i]
                index += 1

                unhook(child, patch, index)

                if (isVNode(child) && child.count) {
                    index += child.count
                }
            }
        }
    } else if (isThunk(vNode)) {
        thunks(vNode, null, patch, index)
    }
}

function undefinedKeys(obj) {
    var result = {}

    for (var key in obj) {
        result[key] = undefined
    }

    return result
}

// List diff, naive left to right reordering
function reorder(aChildren, bChildren) {
    // O(M) time, O(M) memory
    var bChildIndex = keyIndex(bChildren)
    var bKeys = bChildIndex.keys
    var bFree = bChildIndex.free

    if (bFree.length === bChildren.length) {
        return {
            children: bChildren,
            moves: null
        }
    }

    // O(N) time, O(N) memory
    var aChildIndex = keyIndex(aChildren)
    var aKeys = aChildIndex.keys
    var aFree = aChildIndex.free

    if (aFree.length === aChildren.length) {
        return {
            children: bChildren,
            moves: null
        }
    }

    // O(MAX(N, M)) memory
    var newChildren = []

    var freeIndex = 0
    var freeCount = bFree.length
    var deletedItems = 0

    // Iterate through a and match a node in b
    // O(N) time,
    for (var i = 0 ; i < aChildren.length; i++) {
        var aItem = aChildren[i]
        var itemIndex

        if (aItem.key) {
            if (bKeys.hasOwnProperty(aItem.key)) {
                // Match up the old keys
                itemIndex = bKeys[aItem.key]
                newChildren.push(bChildren[itemIndex])

            } else {
                // Remove old keyed items
                itemIndex = i - deletedItems++
                newChildren.push(null)
            }
        } else {
            // Match the item in a with the next free item in b
            if (freeIndex < freeCount) {
                itemIndex = bFree[freeIndex++]
                newChildren.push(bChildren[itemIndex])
            } else {
                // There are no free items in b to match with
                // the free items in a, so the extra free nodes
                // are deleted.
                itemIndex = i - deletedItems++
                newChildren.push(null)
            }
        }
    }

    var lastFreeIndex = freeIndex >= bFree.length ?
        bChildren.length :
        bFree[freeIndex]

    // Iterate through b and append any new keys
    // O(M) time
    for (var j = 0; j < bChildren.length; j++) {
        var newItem = bChildren[j]

        if (newItem.key) {
            if (!aKeys.hasOwnProperty(newItem.key)) {
                // Add any new keyed items
                // We are adding new items to the end and then sorting them
                // in place. In future we should insert new items in place.
                newChildren.push(newItem)
            }
        } else if (j >= lastFreeIndex) {
            // Add any leftover non-keyed items
            newChildren.push(newItem)
        }
    }

    var simulate = newChildren.slice()
    var simulateIndex = 0
    var removes = []
    var inserts = []
    var simulateItem

    for (var k = 0; k < bChildren.length;) {
        var wantedItem = bChildren[k]
        simulateItem = simulate[simulateIndex]

        // remove items
        while (simulateItem === null && simulate.length) {
            removes.push(remove(simulate, simulateIndex, null))
            simulateItem = simulate[simulateIndex]
        }

        if (!simulateItem || simulateItem.key !== wantedItem.key) {
            // if we need a key in this position...
            if (wantedItem.key) {
                if (simulateItem && simulateItem.key) {
                    // if an insert doesn't put this key in place, it needs to move
                    if (bKeys[simulateItem.key] !== k + 1) {
                        removes.push(remove(simulate, simulateIndex, simulateItem.key))
                        simulateItem = simulate[simulateIndex]
                        // if the remove didn't put the wanted item in place, we need to insert it
                        if (!simulateItem || simulateItem.key !== wantedItem.key) {
                            inserts.push({key: wantedItem.key, to: k})
                        }
                        // items are matching, so skip ahead
                        else {
                            simulateIndex++
                        }
                    }
                    else {
                        inserts.push({key: wantedItem.key, to: k})
                    }
                }
                else {
                    inserts.push({key: wantedItem.key, to: k})
                }
                k++
            }
            // a key in simulate has no matching wanted key, remove it
            else if (simulateItem && simulateItem.key) {
                removes.push(remove(simulate, simulateIndex, simulateItem.key))
            }
        }
        else {
            simulateIndex++
            k++
        }
    }

    // remove all the remaining nodes from simulate
    while(simulateIndex < simulate.length) {
        simulateItem = simulate[simulateIndex]
        removes.push(remove(simulate, simulateIndex, simulateItem && simulateItem.key))
    }

    // If the only moves we have are deletes then we can just
    // let the delete patch remove these items.
    if (removes.length === deletedItems && !inserts.length) {
        return {
            children: newChildren,
            moves: null
        }
    }

    return {
        children: newChildren,
        moves: {
            removes: removes,
            inserts: inserts
        }
    }
}

function remove(arr, index, key) {
    arr.splice(index, 1)

    return {
        from: index,
        key: key
    }
}

function keyIndex(children) {
    var keys = {}
    var free = []
    var length = children.length

    for (var i = 0; i < length; i++) {
        var child = children[i]

        if (child.key) {
            keys[child.key] = i
        } else {
            free.push(i)
        }
    }

    return {
        keys: keys,     // A hash of key name to index
        free: free      // An array of unkeyed item indices
    }
}

function appendPatch(apply, patch) {
    if (apply) {
        if (isArray(apply)) {
            apply.push(patch)
        } else {
            apply = [apply, patch]
        }

        return apply
    } else {
        return patch
    }
}

},{"../vnode/handle-thunk":13,"../vnode/is-thunk":14,"../vnode/is-vnode":16,"../vnode/is-vtext":17,"../vnode/is-widget":18,"../vnode/vpatch":21,"./diff-props":23,"x-is-array":25}],25:[function(require,module,exports){
var nativeIsArray = Array.isArray
var toString = Object.prototype.toString

module.exports = nativeIsArray || isArray

function isArray(obj) {
    return toString.call(obj) === "[object Array]"
}

},{}],26:[function(require,module,exports){
// Generated by psc-bundle 0.8.5.0
var PS = {};
(function(exports) {
  /* global exports */
  "use strict";

  // module Prelude

  //- Functor --------------------------------------------------------------------

  exports.arrayMap = function (f) {
    return function (arr) {
      var l = arr.length;
      var result = new Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(arr[i]);
      }
      return result;
    };
  };

  exports.concatArray = function (xs) {
    return function (ys) {
      return xs.concat(ys);
    };
  };

  //- Eq -------------------------------------------------------------------------

  exports.refEq = function (r1) {
    return function (r2) {
      return r1 === r2;
    };
  };

  //- Ord ------------------------------------------------------------------------

  exports.unsafeCompareImpl = function (lt) {
    return function (eq) {
      return function (gt) {
        return function (x) {
          return function (y) {
            return x < y ? lt : x > y ? gt : eq;
          };
        };
      };
    };
  };                                          

  //- BooleanAlgebra -------------------------------------------------------------

  exports.boolOr = function (b1) {
    return function (b2) {
      return b1 || b2;
    };
  };

  exports.boolAnd = function (b1) {
    return function (b2) {
      return b1 && b2;
    };
  };

  exports.boolNot = function (b) {
    return !b;
  };

  //- Show -----------------------------------------------------------------------

  exports.showIntImpl = function (n) {
    return n.toString();
  };

  exports.showStringImpl = function (s) {
    var l = s.length;
    return "\"" + s.replace(
      /[\0-\x1F\x7F"\\]/g,
      function (c, i) { // jshint ignore:line
        switch (c) {
          case "\"":
          case "\\":
            return "\\" + c;
          case "\x07": return "\\a";
          case "\b": return "\\b";
          case "\f": return "\\f";
          case "\n": return "\\n";
          case "\r": return "\\r";
          case "\t": return "\\t";
          case "\v": return "\\v";
        }
        var k = i + 1;
        var empty = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
        return "\\" + c.charCodeAt(0).toString(10) + empty;
      }
    ) + "\"";
  };

  exports.showArrayImpl = function (f) {
    return function (xs) {
      var ss = [];
      for (var i = 0, l = xs.length; i < l; i++) {
        ss[i] = f(xs[i]);
      }
      return "[" + ss.join(",") + "]";
    };
  };
})(PS["Prelude"] = PS["Prelude"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["Prelude"];
  var LT = (function () {
      function LT() {

      };
      LT.value = new LT();
      return LT;
  })();
  var GT = (function () {
      function GT() {

      };
      GT.value = new GT();
      return GT;
  })();
  var EQ = (function () {
      function EQ() {

      };
      EQ.value = new EQ();
      return EQ;
  })();
  var Semigroupoid = function (compose) {
      this.compose = compose;
  };
  var Category = function (__superclass_Prelude$dotSemigroupoid_0, id) {
      this["__superclass_Prelude.Semigroupoid_0"] = __superclass_Prelude$dotSemigroupoid_0;
      this.id = id;
  };
  var Functor = function (map) {
      this.map = map;
  };
  var Apply = function (__superclass_Prelude$dotFunctor_0, apply) {
      this["__superclass_Prelude.Functor_0"] = __superclass_Prelude$dotFunctor_0;
      this.apply = apply;
  };
  var Applicative = function (__superclass_Prelude$dotApply_0, pure) {
      this["__superclass_Prelude.Apply_0"] = __superclass_Prelude$dotApply_0;
      this.pure = pure;
  };
  var Bind = function (__superclass_Prelude$dotApply_0, bind) {
      this["__superclass_Prelude.Apply_0"] = __superclass_Prelude$dotApply_0;
      this.bind = bind;
  };
  var Monad = function (__superclass_Prelude$dotApplicative_0, __superclass_Prelude$dotBind_1) {
      this["__superclass_Prelude.Applicative_0"] = __superclass_Prelude$dotApplicative_0;
      this["__superclass_Prelude.Bind_1"] = __superclass_Prelude$dotBind_1;
  };
  var Semigroup = function (append) {
      this.append = append;
  };
  var Eq = function (eq) {
      this.eq = eq;
  };
  var Ord = function (__superclass_Prelude$dotEq_0, compare) {
      this["__superclass_Prelude.Eq_0"] = __superclass_Prelude$dotEq_0;
      this.compare = compare;
  };
  var Bounded = function (bottom, top) {
      this.bottom = bottom;
      this.top = top;
  };
  var BooleanAlgebra = function (__superclass_Prelude$dotBounded_0, conj, disj, not) {
      this["__superclass_Prelude.Bounded_0"] = __superclass_Prelude$dotBounded_0;
      this.conj = conj;
      this.disj = disj;
      this.not = not;
  };
  var Show = function (show) {
      this.show = show;
  };
  var unsafeCompare = $foreign.unsafeCompareImpl(LT.value)(EQ.value)(GT.value);
  var unit = {};
  var top = function (dict) {
      return dict.top;
  }; 
  var showString = new Show($foreign.showStringImpl);
  var showInt = new Show($foreign.showIntImpl);
  var show = function (dict) {
      return dict.show;
  };
  var showArray = function (dictShow) {
      return new Show($foreign.showArrayImpl(show(dictShow)));
  };                                                                     
  var semigroupoidFn = new Semigroupoid(function (f) {
      return function (g) {
          return function (x) {
              return f(g(x));
          };
      };
  });
  var semigroupArray = new Semigroup($foreign.concatArray);
  var pure = function (dict) {
      return dict.pure;
  };
  var $$return = function (dictApplicative) {
      return pure(dictApplicative);
  };
  var otherwise = true;
  var not = function (dict) {
      return dict.not;
  };
  var map = function (dict) {
      return dict.map;
  };
  var $less$dollar$greater = function (dictFunctor) {
      return map(dictFunctor);
  };
  var id = function (dict) {
      return dict.id;
  };
  var functorArray = new Functor($foreign.arrayMap);
  var flip = function (f) {
      return function (b) {
          return function (a) {
              return f(a)(b);
          };
      };
  }; 
  var eqString = new Eq($foreign.refEq);
  var ordString = new Ord(function () {
      return eqString;
  }, unsafeCompare);
  var eqInt = new Eq($foreign.refEq);
  var ordInt = new Ord(function () {
      return eqInt;
  }, unsafeCompare);
  var eq = function (dict) {
      return dict.eq;
  };
  var disj = function (dict) {
      return dict.disj;
  };
  var $$const = function (a) {
      return function (v) {
          return a;
      };
  };
  var $$void = function (dictFunctor) {
      return function (fa) {
          return $less$dollar$greater(dictFunctor)($$const(unit))(fa);
      };
  };
  var conj = function (dict) {
      return dict.conj;
  };
  var compose = function (dict) {
      return dict.compose;
  };
  var functorFn = new Functor(compose(semigroupoidFn));
  var compare = function (dict) {
      return dict.compare;
  };
  var categoryFn = new Category(function () {
      return semigroupoidFn;
  }, function (x) {
      return x;
  });
  var boundedBoolean = new Bounded(false, true);
  var bottom = function (dict) {
      return dict.bottom;
  };
  var booleanAlgebraBoolean = new BooleanAlgebra(function () {
      return boundedBoolean;
  }, $foreign.boolAnd, $foreign.boolOr, $foreign.boolNot);
  var bind = function (dict) {
      return dict.bind;
  };
  var liftM1 = function (dictMonad) {
      return function (f) {
          return function (a) {
              return bind(dictMonad["__superclass_Prelude.Bind_1"]())(a)(function (v) {
                  return $$return(dictMonad["__superclass_Prelude.Applicative_0"]())(f(v));
              });
          };
      };
  };
  var $greater$greater$eq = function (dictBind) {
      return bind(dictBind);
  }; 
  var apply = function (dict) {
      return dict.apply;
  };
  var $less$times$greater = function (dictApply) {
      return apply(dictApply);
  };
  var liftA1 = function (dictApplicative) {
      return function (f) {
          return function (a) {
              return $less$times$greater(dictApplicative["__superclass_Prelude.Apply_0"]())(pure(dictApplicative)(f))(a);
          };
      };
  }; 
  var append = function (dict) {
      return dict.append;
  };
  var $plus$plus = function (dictSemigroup) {
      return append(dictSemigroup);
  };
  var $less$greater = function (dictSemigroup) {
      return append(dictSemigroup);
  };
  var semigroupFn = function (dictSemigroup) {
      return new Semigroup(function (f) {
          return function (g) {
              return function (x) {
                  return $less$greater(dictSemigroup)(f(x))(g(x));
              };
          };
      });
  };
  var ap = function (dictMonad) {
      return function (f) {
          return function (a) {
              return bind(dictMonad["__superclass_Prelude.Bind_1"]())(f)(function (v) {
                  return bind(dictMonad["__superclass_Prelude.Bind_1"]())(a)(function (v1) {
                      return $$return(dictMonad["__superclass_Prelude.Applicative_0"]())(v(v1));
                  });
              });
          };
      };
  };
  exports["LT"] = LT;
  exports["GT"] = GT;
  exports["EQ"] = EQ;
  exports["Show"] = Show;
  exports["BooleanAlgebra"] = BooleanAlgebra;
  exports["Bounded"] = Bounded;
  exports["Ord"] = Ord;
  exports["Eq"] = Eq;
  exports["Semigroup"] = Semigroup;
  exports["Monad"] = Monad;
  exports["Bind"] = Bind;
  exports["Applicative"] = Applicative;
  exports["Apply"] = Apply;
  exports["Functor"] = Functor;
  exports["Category"] = Category;
  exports["Semigroupoid"] = Semigroupoid;
  exports["show"] = show;
  exports["not"] = not;
  exports["disj"] = disj;
  exports["conj"] = conj;
  exports["bottom"] = bottom;
  exports["top"] = top;
  exports["unsafeCompare"] = unsafeCompare;
  exports["compare"] = compare;
  exports["eq"] = eq;
  exports["++"] = $plus$plus;
  exports["<>"] = $less$greater;
  exports["append"] = append;
  exports["ap"] = ap;
  exports["liftM1"] = liftM1;
  exports["return"] = $$return;
  exports[">>="] = $greater$greater$eq;
  exports["bind"] = bind;
  exports["liftA1"] = liftA1;
  exports["pure"] = pure;
  exports["<*>"] = $less$times$greater;
  exports["apply"] = apply;
  exports["void"] = $$void;
  exports["<$>"] = $less$dollar$greater;
  exports["map"] = map;
  exports["id"] = id;
  exports["compose"] = compose;
  exports["otherwise"] = otherwise;
  exports["const"] = $$const;
  exports["flip"] = flip;
  exports["unit"] = unit;
  exports["semigroupoidFn"] = semigroupoidFn;
  exports["categoryFn"] = categoryFn;
  exports["functorFn"] = functorFn;
  exports["functorArray"] = functorArray;
  exports["semigroupFn"] = semigroupFn;
  exports["semigroupArray"] = semigroupArray;
  exports["eqInt"] = eqInt;
  exports["eqString"] = eqString;
  exports["ordInt"] = ordInt;
  exports["ordString"] = ordString;
  exports["boundedBoolean"] = boundedBoolean;
  exports["booleanAlgebraBoolean"] = booleanAlgebraBoolean;
  exports["showInt"] = showInt;
  exports["showString"] = showString;
  exports["showArray"] = showArray;
})(PS["Prelude"] = PS["Prelude"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  // module Data.Foldable

  exports.foldrArray = function (f) {
    return function (init) {
      return function (xs) {
        var acc = init;
        var len = xs.length;
        for (var i = len - 1; i >= 0; i--) {
          acc = f(xs[i])(acc);
        }
        return acc;
      };
    };
  };

  exports.foldlArray = function (f) {
    return function (init) {
      return function (xs) {
        var acc = init;
        var len = xs.length;
        for (var i = 0; i < len; i++) {
          acc = f(acc)(xs[i]);
        }
        return acc;
      };
    };
  };
})(PS["Data.Foldable"] = PS["Data.Foldable"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var $times$greater = function (dictApply) {
      return function (a) {
          return function (b) {
              return Prelude["<*>"](dictApply)(Prelude["<$>"](dictApply["__superclass_Prelude.Functor_0"]())(Prelude["const"](Prelude.id(Prelude.categoryFn)))(a))(b);
          };
      };
  };
  exports["*>"] = $times$greater;
})(PS["Control.Apply"] = PS["Control.Apply"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];        
  var Monoid = function (__superclass_Prelude$dotSemigroup_0, mempty) {
      this["__superclass_Prelude.Semigroup_0"] = __superclass_Prelude$dotSemigroup_0;
      this.mempty = mempty;
  };     
  var monoidArray = new Monoid(function () {
      return Prelude.semigroupArray;
  }, [  ]);
  var mempty = function (dict) {
      return dict.mempty;
  };
  var monoidFn = function (dictMonoid) {
      return new Monoid(function () {
          return Prelude.semigroupFn(dictMonoid["__superclass_Prelude.Semigroup_0"]());
      }, Prelude["const"](mempty(dictMonoid)));
  };
  exports["Monoid"] = Monoid;
  exports["mempty"] = mempty;
  exports["monoidFn"] = monoidFn;
  exports["monoidArray"] = monoidArray;
})(PS["Data.Monoid"] = PS["Data.Monoid"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Control_Alt = PS["Control.Alt"];
  var Control_Alternative = PS["Control.Alternative"];
  var Control_Extend = PS["Control.Extend"];
  var Control_MonadPlus = PS["Control.MonadPlus"];
  var Control_Plus = PS["Control.Plus"];
  var Data_Functor_Invariant = PS["Data.Functor.Invariant"];
  var Data_Monoid = PS["Data.Monoid"];        
  var Nothing = (function () {
      function Nothing() {

      };
      Nothing.value = new Nothing();
      return Nothing;
  })();
  var Just = (function () {
      function Just(value0) {
          this.value0 = value0;
      };
      Just.create = function (value0) {
          return new Just(value0);
      };
      return Just;
  })();
  var maybe = function (v) {
      return function (v1) {
          return function (v2) {
              if (v2 instanceof Nothing) {
                  return v;
              };
              if (v2 instanceof Just) {
                  return v1(v2.value0);
              };
              throw new Error("Failed pattern match at Data.Maybe line 27, column 1 - line 28, column 1: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
          };
      };
  };
  var isNothing = maybe(true)(Prelude["const"](false));
  var functorMaybe = new Prelude.Functor(function (v) {
      return function (v1) {
          if (v1 instanceof Just) {
              return new Just(v(v1.value0));
          };
          return Nothing.value;
      };
  });
  var fromMaybe = function (a) {
      return maybe(a)(Prelude.id(Prelude.categoryFn));
  };
  var applyMaybe = new Prelude.Apply(function () {
      return functorMaybe;
  }, function (v) {
      return function (v1) {
          if (v instanceof Just) {
              return Prelude["<$>"](functorMaybe)(v.value0)(v1);
          };
          if (v instanceof Nothing) {
              return Nothing.value;
          };
          throw new Error("Failed pattern match at Data.Maybe line 122, column 3 - line 123, column 3: " + [ v.constructor.name, v1.constructor.name ]);
      };
  });
  var bindMaybe = new Prelude.Bind(function () {
      return applyMaybe;
  }, function (v) {
      return function (v1) {
          if (v instanceof Just) {
              return v1(v.value0);
          };
          if (v instanceof Nothing) {
              return Nothing.value;
          };
          throw new Error("Failed pattern match at Data.Maybe line 181, column 3 - line 182, column 3: " + [ v.constructor.name, v1.constructor.name ]);
      };
  });
  var applicativeMaybe = new Prelude.Applicative(function () {
      return applyMaybe;
  }, Just.create);
  exports["Nothing"] = Nothing;
  exports["Just"] = Just;
  exports["isNothing"] = isNothing;
  exports["fromMaybe"] = fromMaybe;
  exports["maybe"] = maybe;
  exports["functorMaybe"] = functorMaybe;
  exports["applyMaybe"] = applyMaybe;
  exports["applicativeMaybe"] = applicativeMaybe;
  exports["bindMaybe"] = bindMaybe;
})(PS["Data.Maybe"] = PS["Data.Maybe"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Control_Comonad = PS["Control.Comonad"];
  var Control_Extend = PS["Control.Extend"];
  var Data_Monoid = PS["Data.Monoid"];        
  var Disj = function (x) {
      return x;
  };
  var semigroupDisj = function (dictBooleanAlgebra) {
      return new Prelude.Semigroup(function (v) {
          return function (v1) {
              return Prelude.disj(dictBooleanAlgebra)(v)(v1);
          };
      });
  };
  var runDisj = function (v) {
      return v;
  };
  var monoidDisj = function (dictBooleanAlgebra) {
      return new Data_Monoid.Monoid(function () {
          return semigroupDisj(dictBooleanAlgebra);
      }, Prelude.bottom(dictBooleanAlgebra["__superclass_Prelude.Bounded_0"]()));
  };
  exports["Disj"] = Disj;
  exports["runDisj"] = runDisj;
  exports["semigroupDisj"] = semigroupDisj;
  exports["monoidDisj"] = monoidDisj;
})(PS["Data.Monoid.Disj"] = PS["Data.Monoid.Disj"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["Data.Foldable"];
  var Prelude = PS["Prelude"];
  var Control_Apply = PS["Control.Apply"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Maybe_First = PS["Data.Maybe.First"];
  var Data_Maybe_Last = PS["Data.Maybe.Last"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_Monoid_Additive = PS["Data.Monoid.Additive"];
  var Data_Monoid_Conj = PS["Data.Monoid.Conj"];
  var Data_Monoid_Disj = PS["Data.Monoid.Disj"];
  var Data_Monoid_Dual = PS["Data.Monoid.Dual"];
  var Data_Monoid_Endo = PS["Data.Monoid.Endo"];
  var Data_Monoid_Multiplicative = PS["Data.Monoid.Multiplicative"];        
  var Foldable = function (foldMap, foldl, foldr) {
      this.foldMap = foldMap;
      this.foldl = foldl;
      this.foldr = foldr;
  };
  var foldr = function (dict) {
      return dict.foldr;
  };
  var traverse_ = function (dictApplicative) {
      return function (dictFoldable) {
          return function (f) {
              return foldr(dictFoldable)(function ($161) {
                  return Control_Apply["*>"](dictApplicative["__superclass_Prelude.Apply_0"]())(f($161));
              })(Prelude.pure(dictApplicative)(Prelude.unit));
          };
      };
  };
  var for_ = function (dictApplicative) {
      return function (dictFoldable) {
          return Prelude.flip(traverse_(dictApplicative)(dictFoldable));
      };
  };
  var foldl = function (dict) {
      return dict.foldl;
  };
  var mconcat = function (dictFoldable) {
      return function (dictMonoid) {
          return foldl(dictFoldable)(Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]()))(Data_Monoid.mempty(dictMonoid));
      };
  }; 
  var foldMapDefaultR = function (dictFoldable) {
      return function (dictMonoid) {
          return function (f) {
              return function (xs) {
                  return foldr(dictFoldable)(function (x) {
                      return function (acc) {
                          return Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(f(x))(acc);
                      };
                  })(Data_Monoid.mempty(dictMonoid))(xs);
              };
          };
      };
  };
  var foldableArray = new Foldable(function (dictMonoid) {
      return foldMapDefaultR(foldableArray)(dictMonoid);
  }, $foreign.foldlArray, $foreign.foldrArray);
  var foldMap = function (dict) {
      return dict.foldMap;
  };
  var find = function (dictFoldable) {
      return function (p) {
          return foldl(dictFoldable)(function (r) {
              return function (x) {
                  var $160 = p(x);
                  if ($160) {
                      return new Data_Maybe.Just(x);
                  };
                  if (!$160) {
                      return r;
                  };
                  throw new Error("Failed pattern match at Data.Foldable line 234, column 25 - line 234, column 50: " + [ $160.constructor.name ]);
              };
          })(Data_Maybe.Nothing.value);
      };
  };
  var any = function (dictFoldable) {
      return function (dictBooleanAlgebra) {
          return function (p) {
              return function ($164) {
                  return Data_Monoid_Disj.runDisj(foldMap(dictFoldable)(Data_Monoid_Disj.monoidDisj(dictBooleanAlgebra))(function ($165) {
                      return Data_Monoid_Disj.Disj(p($165));
                  })($164));
              };
          };
      };
  };
  exports["Foldable"] = Foldable;
  exports["find"] = find;
  exports["any"] = any;
  exports["mconcat"] = mconcat;
  exports["for_"] = for_;
  exports["traverse_"] = traverse_;
  exports["foldMapDefaultR"] = foldMapDefaultR;
  exports["foldMap"] = foldMap;
  exports["foldl"] = foldl;
  exports["foldr"] = foldr;
  exports["foldableArray"] = foldableArray;
})(PS["Data.Foldable"] = PS["Data.Foldable"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  // module Data.Traversable

  // jshint maxparams: 3

  exports.traverseArrayImpl = function () {
    function Cont (fn) {
      this.fn = fn;
    }

    var emptyList = {};

    var ConsCell = function (head, tail) {
      this.head = head;
      this.tail = tail;
    };

    function consList (x) {
      return function (xs) {
        return new ConsCell(x, xs);
      };
    }

    function listToArray (list) {
      var arr = [];
      while (list !== emptyList) {
        arr.push(list.head);
        list = list.tail;
      }
      return arr;
    }

    return function (apply) {
      return function (map) {
        return function (pure) {
          return function (f) {
            var buildFrom = function (x, ys) {
              return apply(map(consList)(f(x)))(ys);
            };

            var go = function (acc, currentLen, xs) {
              if (currentLen === 0) {
                return acc;
              } else {
                var last = xs[currentLen - 1];
                return new Cont(function () {
                  return go(buildFrom(last, acc), currentLen - 1, xs);
                });
              }
            };

            return function (array) {
              var result = go(pure(emptyList), array.length, array);
              while (result instanceof Cont) {
                result = result.fn();
              }

              return map(listToArray)(result);
            };
          };
        };
      };
    };
  }();
})(PS["Data.Traversable"] = PS["Data.Traversable"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["Data.Traversable"];
  var Prelude = PS["Prelude"];
  var Data_Foldable = PS["Data.Foldable"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Maybe_First = PS["Data.Maybe.First"];
  var Data_Maybe_Last = PS["Data.Maybe.Last"];
  var Data_Monoid_Additive = PS["Data.Monoid.Additive"];
  var Data_Monoid_Conj = PS["Data.Monoid.Conj"];
  var Data_Monoid_Disj = PS["Data.Monoid.Disj"];
  var Data_Monoid_Dual = PS["Data.Monoid.Dual"];
  var Data_Monoid_Multiplicative = PS["Data.Monoid.Multiplicative"];
  var Traversable = function (__superclass_Data$dotFoldable$dotFoldable_1, __superclass_Prelude$dotFunctor_0, sequence, traverse) {
      this["__superclass_Data.Foldable.Foldable_1"] = __superclass_Data$dotFoldable$dotFoldable_1;
      this["__superclass_Prelude.Functor_0"] = __superclass_Prelude$dotFunctor_0;
      this.sequence = sequence;
      this.traverse = traverse;
  };
  var traverse = function (dict) {
      return dict.traverse;
  };
  var sequenceDefault = function (dictTraversable) {
      return function (dictApplicative) {
          return function (tma) {
              return traverse(dictTraversable)(dictApplicative)(Prelude.id(Prelude.categoryFn))(tma);
          };
      };
  };
  var traversableArray = new Traversable(function () {
      return Data_Foldable.foldableArray;
  }, function () {
      return Prelude.functorArray;
  }, function (dictApplicative) {
      return sequenceDefault(traversableArray)(dictApplicative);
  }, function (dictApplicative) {
      return $foreign.traverseArrayImpl(Prelude.apply(dictApplicative["__superclass_Prelude.Apply_0"]()))(Prelude.map((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]()))(Prelude.pure(dictApplicative));
  });
  var sequence = function (dict) {
      return dict.sequence;
  }; 
  var $$for = function (dictApplicative) {
      return function (dictTraversable) {
          return function (x) {
              return function (f) {
                  return traverse(dictTraversable)(dictApplicative)(f)(x);
              };
          };
      };
  };
  exports["Traversable"] = Traversable;
  exports["for"] = $$for;
  exports["sequenceDefault"] = sequenceDefault;
  exports["sequence"] = sequence;
  exports["traverse"] = traverse;
  exports["traversableArray"] = traversableArray;
})(PS["Data.Traversable"] = PS["Data.Traversable"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Control_Alt = PS["Control.Alt"];
  var Control_Extend = PS["Control.Extend"];
  var Data_Bifoldable = PS["Data.Bifoldable"];
  var Data_Bifunctor = PS["Data.Bifunctor"];
  var Data_Bitraversable = PS["Data.Bitraversable"];
  var Data_Foldable = PS["Data.Foldable"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_Traversable = PS["Data.Traversable"];        
  var Left = (function () {
      function Left(value0) {
          this.value0 = value0;
      };
      Left.create = function (value0) {
          return new Left(value0);
      };
      return Left;
  })();
  var Right = (function () {
      function Right(value0) {
          this.value0 = value0;
      };
      Right.create = function (value0) {
          return new Right(value0);
      };
      return Right;
  })();
  var functorEither = new Prelude.Functor(function (v) {
      return function (v1) {
          if (v1 instanceof Left) {
              return new Left(v1.value0);
          };
          if (v1 instanceof Right) {
              return new Right(v(v1.value0));
          };
          throw new Error("Failed pattern match at Data.Either line 53, column 3 - line 54, column 3: " + [ v.constructor.name, v1.constructor.name ]);
      };
  });
  var either = function (v) {
      return function (v1) {
          return function (v2) {
              if (v2 instanceof Left) {
                  return v(v2.value0);
              };
              if (v2 instanceof Right) {
                  return v1(v2.value0);
              };
              throw new Error("Failed pattern match at Data.Either line 29, column 1 - line 30, column 1: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
          };
      };
  }; 
  var applyEither = new Prelude.Apply(function () {
      return functorEither;
  }, function (v) {
      return function (v1) {
          if (v instanceof Left) {
              return new Left(v.value0);
          };
          if (v instanceof Right) {
              return Prelude["<$>"](functorEither)(v.value0)(v1);
          };
          throw new Error("Failed pattern match at Data.Either line 93, column 3 - line 94, column 3: " + [ v.constructor.name, v1.constructor.name ]);
      };
  });
  var bindEither = new Prelude.Bind(function () {
      return applyEither;
  }, either(function (e) {
      return function (v) {
          return new Left(e);
      };
  })(function (a) {
      return function (f) {
          return f(a);
      };
  }));
  var applicativeEither = new Prelude.Applicative(function () {
      return applyEither;
  }, Right.create);
  exports["Left"] = Left;
  exports["Right"] = Right;
  exports["either"] = either;
  exports["functorEither"] = functorEither;
  exports["applyEither"] = applyEither;
  exports["applicativeEither"] = applicativeEither;
  exports["bindEither"] = bindEither;
})(PS["Data.Either"] = PS["Data.Either"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Control_Biapplicative = PS["Control.Biapplicative"];
  var Control_Biapply = PS["Control.Biapply"];
  var Control_Comonad = PS["Control.Comonad"];
  var Control_Extend = PS["Control.Extend"];
  var Control_Lazy = PS["Control.Lazy"];
  var Data_Bifoldable = PS["Data.Bifoldable"];
  var Data_Bifunctor = PS["Data.Bifunctor"];
  var Data_Bitraversable = PS["Data.Bitraversable"];
  var Data_Foldable = PS["Data.Foldable"];
  var Data_Functor_Invariant = PS["Data.Functor.Invariant"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Maybe_First = PS["Data.Maybe.First"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_Traversable = PS["Data.Traversable"];        
  var Tuple = (function () {
      function Tuple(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      Tuple.create = function (value0) {
          return function (value1) {
              return new Tuple(value0, value1);
          };
      };
      return Tuple;
  })();
  var snd = function (v) {
      return v.value1;
  };
  var functorTuple = new Prelude.Functor(function (f) {
      return function (v) {
          return new Tuple(v.value0, f(v.value1));
      };
  });                                                                                                   
  var fst = function (v) {
      return v.value0;
  };
  exports["Tuple"] = Tuple;
  exports["snd"] = snd;
  exports["fst"] = fst;
  exports["functorTuple"] = functorTuple;
})(PS["Data.Tuple"] = PS["Data.Tuple"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  //------------------------------------------------------------------------------
  // Array size ------------------------------------------------------------------
  //------------------------------------------------------------------------------

  exports.length = function (xs) {
    return xs.length;
  };

  //------------------------------------------------------------------------------
  // Extending arrays ------------------------------------------------------------
  //------------------------------------------------------------------------------

  exports.cons = function (e) {
    return function (l) {
      return [e].concat(l);
    };
  };

  exports.snoc = function (l) {
    return function (e) {
      var l1 = l.slice();
      l1.push(e);
      return l1;
    };
  };

  //------------------------------------------------------------------------------
  // Non-indexed reads -----------------------------------------------------------
  //------------------------------------------------------------------------------

  exports["uncons'"] = function (empty) {
    return function (next) {
      return function (xs) {
        return xs.length === 0 ? empty({}) : next(xs[0])(xs.slice(1));
      };
    };
  };

  //------------------------------------------------------------------------------
  // Indexed operations ----------------------------------------------------------
  //------------------------------------------------------------------------------

  exports.indexImpl = function (just) {
    return function (nothing) {
      return function (xs) {
        return function (i) {
          return i < 0 || i >= xs.length ? nothing :  just(xs[i]);
        };
      };
    };
  };

  exports._deleteAt = function (just) {
    return function (nothing) {
      return function (i) {
        return function (l) {
          if (i < 0 || i >= l.length) return nothing;
          var l1 = l.slice();
          l1.splice(i, 1);
          return just(l1);
        };
      };
    };
  };

  exports._updateAt = function (just) {
    return function (nothing) {
      return function (i) {
        return function (a) {
          return function (l) {
            if (i < 0 || i >= l.length) return nothing;
            var l1 = l.slice();
            l1[i] = a;
            return just(l1);
          };
        };
      };
    };
  };

  exports.concat = function (xss) {
    var result = [];
    for (var i = 0, l = xss.length; i < l; i++) {
      var xs = xss[i];
      for (var j = 0, m = xs.length; j < m; j++) {
        result.push(xs[j]);
      }
    }
    return result;
  };

  //------------------------------------------------------------------------------
  // Sorting ---------------------------------------------------------------------
  //------------------------------------------------------------------------------

  exports.sortImpl = function (f) {
    return function (l) {
      /* jshint maxparams: 2 */
      return l.slice().sort(function (x, y) {
        return f(x)(y);
      });
    };
  };

  //------------------------------------------------------------------------------
  // Subarrays -------------------------------------------------------------------
  //------------------------------------------------------------------------------

  exports.slice = function (s) {
    return function (e) {
      return function (l) {
        return l.slice(s, e);
      };
    };
  };

  //------------------------------------------------------------------------------
  // Zipping ---------------------------------------------------------------------
  //------------------------------------------------------------------------------

  exports.zipWith = function (f) {
    return function (xs) {
      return function (ys) {
        var l = xs.length < ys.length ? xs.length : ys.length;
        var result = new Array(l);
        for (var i = 0; i < l; i++) {
          result[i] = f(xs[i])(ys[i]);
        }
        return result;
      };
    };
  };
})(PS["Data.Array"] = PS["Data.Array"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["Data.Array"];
  var Prelude = PS["Prelude"];
  var Control_Alt = PS["Control.Alt"];
  var Control_Alternative = PS["Control.Alternative"];
  var Control_Lazy = PS["Control.Lazy"];
  var Control_MonadPlus = PS["Control.MonadPlus"];
  var Control_Plus = PS["Control.Plus"];
  var Data_Foldable = PS["Data.Foldable"];
  var Data_Functor_Invariant = PS["Data.Functor.Invariant"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_Traversable = PS["Data.Traversable"];
  var Data_Tuple = PS["Data.Tuple"];
  var Data_Maybe_Unsafe = PS["Data.Maybe.Unsafe"];
  var zipWithA = function (dictApplicative) {
      return function (f) {
          return function (xs) {
              return function (ys) {
                  return Data_Traversable.sequence(Data_Traversable.traversableArray)(dictApplicative)($foreign.zipWith(f)(xs)(ys));
              };
          };
      };
  };                                                  
  var updateAt = $foreign._updateAt(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
  var sortBy = function (comp) {
      return function (xs) {
          var comp$prime = function (x) {
              return function (y) {
                  var $46 = comp(x)(y);
                  if ($46 instanceof Prelude.GT) {
                      return 1;
                  };
                  if ($46 instanceof Prelude.EQ) {
                      return 0;
                  };
                  if ($46 instanceof Prelude.LT) {
                      return -1;
                  };
                  throw new Error("Failed pattern match at Data.Array line 417, column 15 - line 422, column 1: " + [ $46.constructor.name ]);
              };
          };
          return $foreign.sortImpl(comp$prime)(xs);
      };
  };
  var index = $foreign.indexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
  var $bang$bang = index;
  var foldM = function (dictMonad) {
      return function (f) {
          return function (a) {
              return $foreign["uncons'"](function (v) {
                  return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(a);
              })(function (b) {
                  return function (bs) {
                      return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(f(a)(b))(function (a$prime) {
                          return foldM(dictMonad)(f)(a$prime)(bs);
                      });
                  };
              });
          };
      };
  };
  var deleteAt = $foreign._deleteAt(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
  exports["foldM"] = foldM;
  exports["zipWithA"] = zipWithA;
  exports["sortBy"] = sortBy;
  exports["updateAt"] = updateAt;
  exports["deleteAt"] = deleteAt;
  exports["index"] = index;
  exports["!!"] = $bang$bang;
  exports["zipWith"] = $foreign.zipWith;
  exports["snoc"] = $foreign.snoc;
  exports["cons"] = $foreign.cons;
  exports["length"] = $foreign.length;
})(PS["Data.Array"] = PS["Data.Array"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  exports._toChar = function (just) {
    return function (nothing) {
      return function (s) {
        return s.length === 1 ? just(s) : nothing;
      };
    };
  };

  exports.length = function (s) {
    return s.length;
  };

  exports.take = function (n) {
    return function (s) {
      return s.substr(0, n);
    };
  };

  exports.drop = function (n) {
    return function (s) {
      return s.substr(n);
    };
  };
})(PS["Data.String"] = PS["Data.String"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["Data.String"];
  var Prelude = PS["Prelude"];
  var Data_Char = PS["Data.Char"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_String_Unsafe = PS["Data.String.Unsafe"];
  var toChar = $foreign._toChar(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
  exports["toChar"] = toChar;
  exports["drop"] = $foreign.drop;
  exports["take"] = $foreign.take;
})(PS["Data.String"] = PS["Data.String"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $$Proxy = (function () {
      function Proxy() {

      };
      Proxy.value = new Proxy();
      return Proxy;
  })();
  exports["Proxy"] = $$Proxy;
})(PS["Type.Proxy"] = PS["Type.Proxy"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["Data.Generic"];
  var Prelude = PS["Prelude"];
  var Data_Either = PS["Data.Either"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Tuple = PS["Data.Tuple"];
  var Data_Traversable = PS["Data.Traversable"];
  var Data_Foldable = PS["Data.Foldable"];
  var Data_Array = PS["Data.Array"];
  var Data_String = PS["Data.String"];
  var Type_Proxy = PS["Type.Proxy"];        
  var SProd = (function () {
      function SProd(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      SProd.create = function (value0) {
          return function (value1) {
              return new SProd(value0, value1);
          };
      };
      return SProd;
  })();
  var SRecord = (function () {
      function SRecord(value0) {
          this.value0 = value0;
      };
      SRecord.create = function (value0) {
          return new SRecord(value0);
      };
      return SRecord;
  })();
  var SNumber = (function () {
      function SNumber(value0) {
          this.value0 = value0;
      };
      SNumber.create = function (value0) {
          return new SNumber(value0);
      };
      return SNumber;
  })();
  var SBoolean = (function () {
      function SBoolean(value0) {
          this.value0 = value0;
      };
      SBoolean.create = function (value0) {
          return new SBoolean(value0);
      };
      return SBoolean;
  })();
  var SInt = (function () {
      function SInt(value0) {
          this.value0 = value0;
      };
      SInt.create = function (value0) {
          return new SInt(value0);
      };
      return SInt;
  })();
  var SString = (function () {
      function SString(value0) {
          this.value0 = value0;
      };
      SString.create = function (value0) {
          return new SString(value0);
      };
      return SString;
  })();
  var SChar = (function () {
      function SChar(value0) {
          this.value0 = value0;
      };
      SChar.create = function (value0) {
          return new SChar(value0);
      };
      return SChar;
  })();
  var SArray = (function () {
      function SArray(value0) {
          this.value0 = value0;
      };
      SArray.create = function (value0) {
          return new SArray(value0);
      };
      return SArray;
  })();
  var SigProd = (function () {
      function SigProd(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      SigProd.create = function (value0) {
          return function (value1) {
              return new SigProd(value0, value1);
          };
      };
      return SigProd;
  })();
  var SigRecord = (function () {
      function SigRecord(value0) {
          this.value0 = value0;
      };
      SigRecord.create = function (value0) {
          return new SigRecord(value0);
      };
      return SigRecord;
  })();
  var SigNumber = (function () {
      function SigNumber() {

      };
      SigNumber.value = new SigNumber();
      return SigNumber;
  })();
  var SigBoolean = (function () {
      function SigBoolean() {

      };
      SigBoolean.value = new SigBoolean();
      return SigBoolean;
  })();
  var SigInt = (function () {
      function SigInt() {

      };
      SigInt.value = new SigInt();
      return SigInt;
  })();
  var SigString = (function () {
      function SigString() {

      };
      SigString.value = new SigString();
      return SigString;
  })();
  var SigChar = (function () {
      function SigChar() {

      };
      SigChar.value = new SigChar();
      return SigChar;
  })();
  var SigArray = (function () {
      function SigArray(value0) {
          this.value0 = value0;
      };
      SigArray.create = function (value0) {
          return new SigArray(value0);
      };
      return SigArray;
  })();
  var Generic = function (fromSpine, toSignature, toSpine) {
      this.fromSpine = fromSpine;
      this.toSignature = toSignature;
      this.toSpine = toSpine;
  };
  var toSpine = function (dict) {
      return dict.toSpine;
  };
  var toSignature = function (dict) {
      return dict.toSignature;
  };
  var genericUnit = new Generic(function (v) {
      if (v instanceof SProd && (v.value0 === "Prelude.Unit" && v.value1.length === 0)) {
          return new Data_Maybe.Just(Prelude.unit);
      };
      return Data_Maybe.Nothing.value;
  }, function (x) {
      return new SigProd("Prelude.Unit", [ {
          sigConstructor: "Prelude.Unit", 
          sigValues: [  ]
      } ]);
  }, function (x) {
      return new SProd("Prelude.Unit", [  ]);
  });
  var genericString = new Generic(function (v) {
      if (v instanceof SString) {
          return new Data_Maybe.Just(v.value0);
      };
      return Data_Maybe.Nothing.value;
  }, function (v) {
      return SigString.value;
  }, function (x) {
      return new SString(x);
  });
  var genericNumber = new Generic(function (v) {
      if (v instanceof SNumber) {
          return new Data_Maybe.Just(v.value0);
      };
      return Data_Maybe.Nothing.value;
  }, function (v) {
      return SigNumber.value;
  }, function (x) {
      return new SNumber(x);
  });
  var genericInt = new Generic(function (v) {
      if (v instanceof SInt) {
          return new Data_Maybe.Just(v.value0);
      };
      return Data_Maybe.Nothing.value;
  }, function (v) {
      return SigInt.value;
  }, function (x) {
      return new SInt(x);
  });
  var fromSpine = function (dict) {
      return dict.fromSpine;
  };
  var anyProxy = (Type_Proxy["Proxy"]).value;
  var genericArray = function (dictGeneric) {
      return new Generic(function (v) {
          if (v instanceof SArray) {
              return Data_Traversable.traverse(Data_Traversable.traversableArray)(Data_Maybe.applicativeMaybe)(function ($188) {
                  return fromSpine(dictGeneric)((function (v1) {
                      return v1(Prelude.unit);
                  })($188));
              })(v.value0);
          };
          return Data_Maybe.Nothing.value;
      }, function (x) {
          var lowerProxy = function (v) {
              return anyProxy;
          };
          return new SigArray(function (unit) {
              return toSignature(dictGeneric)(lowerProxy(x));
          });
      }, function (xs) {
          return new SArray(Prelude["<$>"](Prelude.functorArray)(function (x) {
              return function (y) {
                  return toSpine(dictGeneric)(x);
              };
          })(xs));
      });
  };
  var genericMaybe = function (dictGeneric) {
      return new Generic(function (v) {
          if (v instanceof SProd && (v.value0 === "Data.Maybe.Just" && v.value1.length === 1)) {
              return Prelude["<$>"](Data_Maybe.functorMaybe)(Data_Maybe.Just.create)(fromSpine(dictGeneric)(v.value1[0](Prelude.unit)));
          };
          if (v instanceof SProd && (v.value0 === "Data.Maybe.Nothing" && v.value1.length === 0)) {
              return Prelude["return"](Data_Maybe.applicativeMaybe)(Data_Maybe.Nothing.value);
          };
          return Data_Maybe.Nothing.value;
      }, function (x) {
          var mbProxy = function (v) {
              return anyProxy;
          };
          return new SigProd("Data.Maybe.Maybe", [ {
              sigConstructor: "Data.Maybe.Just", 
              sigValues: [ function (u) {
                  return toSignature(dictGeneric)(mbProxy(x));
              } ]
          }, {
              sigConstructor: "Data.Maybe.Nothing", 
              sigValues: [  ]
          } ]);
      }, function (v) {
          if (v instanceof Data_Maybe.Just) {
              return new SProd("Data.Maybe.Just", [ function (u) {
                  return toSpine(dictGeneric)(v.value0);
              } ]);
          };
          if (v instanceof Data_Maybe.Nothing) {
              return new SProd("Data.Maybe.Nothing", [  ]);
          };
          throw new Error("Failed pattern match at Data.Generic line 137, column 7 - line 138, column 7: " + [ v.constructor.name ]);
      });
  };
  var genericTuple = function (dictGeneric) {
      return function (dictGeneric1) {
          return new Generic(function (v) {
              if (v instanceof SProd && (v.value0 === "Data.Tuple.Tuple" && v.value1.length === 2)) {
                  return Prelude["<*>"](Data_Maybe.applyMaybe)(Prelude["<$>"](Data_Maybe.functorMaybe)(Data_Tuple.Tuple.create)(fromSpine(dictGeneric)(v.value1[0](Prelude.unit))))(fromSpine(dictGeneric1)(v.value1[1](Prelude.unit)));
              };
              return Data_Maybe.Nothing.value;
          }, function (x) {
              var sndProxy = function (v) {
                  return anyProxy;
              };
              var fstProxy = function (v) {
                  return anyProxy;
              };
              return new SigProd("Data.Tuple.Tuple", [ {
                  sigConstructor: "Data.Tuple.Tuple", 
                  sigValues: [ function (u) {
                      return toSignature(dictGeneric)(fstProxy(x));
                  }, function (u) {
                      return toSignature(dictGeneric1)(sndProxy(x));
                  } ]
              } ]);
          }, function (v) {
              return new SProd("Data.Tuple.Tuple", [ function (u) {
                  return toSpine(dictGeneric)(v.value0);
              }, function (u) {
                  return toSpine(dictGeneric1)(v.value1);
              } ]);
          });
      };
  };
  exports["SigProd"] = SigProd;
  exports["SigRecord"] = SigRecord;
  exports["SigNumber"] = SigNumber;
  exports["SigBoolean"] = SigBoolean;
  exports["SigInt"] = SigInt;
  exports["SigString"] = SigString;
  exports["SigChar"] = SigChar;
  exports["SigArray"] = SigArray;
  exports["SProd"] = SProd;
  exports["SRecord"] = SRecord;
  exports["SNumber"] = SNumber;
  exports["SBoolean"] = SBoolean;
  exports["SInt"] = SInt;
  exports["SString"] = SString;
  exports["SChar"] = SChar;
  exports["SArray"] = SArray;
  exports["Generic"] = Generic;
  exports["fromSpine"] = fromSpine;
  exports["toSignature"] = toSignature;
  exports["toSpine"] = toSpine;
  exports["genericNumber"] = genericNumber;
  exports["genericInt"] = genericInt;
  exports["genericString"] = genericString;
  exports["genericArray"] = genericArray;
  exports["genericTuple"] = genericTuple;
  exports["genericMaybe"] = genericMaybe;
  exports["genericUnit"] = genericUnit;
})(PS["Data.Generic"] = PS["Data.Generic"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Data_Either = PS["Data.Either"];
  var Data_Generic = PS["Data.Generic"];
  var Data_String = PS["Data.String"];
  var Data_Maybe = PS["Data.Maybe"];        
  var OPTIONS = (function () {
      function OPTIONS() {

      };
      OPTIONS.value = new OPTIONS();
      return OPTIONS;
  })();
  var GET = (function () {
      function GET() {

      };
      GET.value = new GET();
      return GET;
  })();
  var HEAD = (function () {
      function HEAD() {

      };
      HEAD.value = new HEAD();
      return HEAD;
  })();
  var POST = (function () {
      function POST() {

      };
      POST.value = new POST();
      return POST;
  })();
  var PUT = (function () {
      function PUT() {

      };
      PUT.value = new PUT();
      return PUT;
  })();
  var DELETE = (function () {
      function DELETE() {

      };
      DELETE.value = new DELETE();
      return DELETE;
  })();
  var TRACE = (function () {
      function TRACE() {

      };
      TRACE.value = new TRACE();
      return TRACE;
  })();
  var CONNECT = (function () {
      function CONNECT() {

      };
      CONNECT.value = new CONNECT();
      return CONNECT;
  })();
  var PROPFIND = (function () {
      function PROPFIND() {

      };
      PROPFIND.value = new PROPFIND();
      return PROPFIND;
  })();
  var PROPPATCH = (function () {
      function PROPPATCH() {

      };
      PROPPATCH.value = new PROPPATCH();
      return PROPPATCH;
  })();
  var MKCOL = (function () {
      function MKCOL() {

      };
      MKCOL.value = new MKCOL();
      return MKCOL;
  })();
  var COPY = (function () {
      function COPY() {

      };
      COPY.value = new COPY();
      return COPY;
  })();
  var MOVE = (function () {
      function MOVE() {

      };
      MOVE.value = new MOVE();
      return MOVE;
  })();
  var LOCK = (function () {
      function LOCK() {

      };
      LOCK.value = new LOCK();
      return LOCK;
  })();
  var UNLOCK = (function () {
      function UNLOCK() {

      };
      UNLOCK.value = new UNLOCK();
      return UNLOCK;
  })();
  var PATCH = (function () {
      function PATCH() {

      };
      PATCH.value = new PATCH();
      return PATCH;
  })();
  var showMethod = new Prelude.Show(function (v) {
      if (v instanceof OPTIONS) {
          return "OPTIONS";
      };
      if (v instanceof GET) {
          return "GET";
      };
      if (v instanceof HEAD) {
          return "HEAD";
      };
      if (v instanceof POST) {
          return "POST";
      };
      if (v instanceof PUT) {
          return "PUT";
      };
      if (v instanceof DELETE) {
          return "DELETE";
      };
      if (v instanceof TRACE) {
          return "TRACE";
      };
      if (v instanceof CONNECT) {
          return "CONNECT";
      };
      if (v instanceof PROPFIND) {
          return "PROPFIND";
      };
      if (v instanceof PROPPATCH) {
          return "PROPPATCH";
      };
      if (v instanceof MKCOL) {
          return "MKCOL";
      };
      if (v instanceof COPY) {
          return "COPY";
      };
      if (v instanceof MOVE) {
          return "MOVE";
      };
      if (v instanceof LOCK) {
          return "LOCK";
      };
      if (v instanceof UNLOCK) {
          return "UNLOCK";
      };
      if (v instanceof PATCH) {
          return "PATCH";
      };
      throw new Error("Failed pattern match at Data.HTTP.Method line 63, column 3 - line 64, column 3: " + [ v.constructor.name ]);
  });
  var runCustomMethod = function (v) {
      return v;
  };
  var print = Data_Either.either(Prelude.show(showMethod))(runCustomMethod);
  exports["OPTIONS"] = OPTIONS;
  exports["GET"] = GET;
  exports["HEAD"] = HEAD;
  exports["POST"] = POST;
  exports["PUT"] = PUT;
  exports["DELETE"] = DELETE;
  exports["TRACE"] = TRACE;
  exports["CONNECT"] = CONNECT;
  exports["PROPFIND"] = PROPFIND;
  exports["PROPPATCH"] = PROPPATCH;
  exports["MKCOL"] = MKCOL;
  exports["COPY"] = COPY;
  exports["MOVE"] = MOVE;
  exports["LOCK"] = LOCK;
  exports["UNLOCK"] = UNLOCK;
  exports["PATCH"] = PATCH;
  exports["print"] = print;
  exports["runCustomMethod"] = runCustomMethod;
  exports["showMethod"] = showMethod;
})(PS["Data.HTTP.Method"] = PS["Data.HTTP.Method"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  exports.nowImpl = function (ctor) {
    return function () {
      return ctor(new Date());
    };
  };

  exports.jsDateConstructor = function (x) {
    return new Date(x);
  };

  // jshint maxparams: 2
  exports.jsDateMethod = function (method, date) {
    return date[method]();
  };
})(PS["Data.Date"] = PS["Data.Date"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  // module Control.Monad.Eff

  exports.returnE = function (a) {
    return function () {
      return a;
    };
  };

  exports.bindE = function (a) {
    return function (f) {
      return function () {
        return f(a())();
      };
    };
  };

  exports.runPure = function (f) {
    return f();
  };
})(PS["Control.Monad.Eff"] = PS["Control.Monad.Eff"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["Control.Monad.Eff"];
  var Prelude = PS["Prelude"];        
  var monadEff = new Prelude.Monad(function () {
      return applicativeEff;
  }, function () {
      return bindEff;
  });
  var bindEff = new Prelude.Bind(function () {
      return applyEff;
  }, $foreign.bindE);
  var applyEff = new Prelude.Apply(function () {
      return functorEff;
  }, Prelude.ap(monadEff));
  var applicativeEff = new Prelude.Applicative(function () {
      return applyEff;
  }, $foreign.returnE);
  var functorEff = new Prelude.Functor(Prelude.liftA1(applicativeEff));
  exports["functorEff"] = functorEff;
  exports["applyEff"] = applyEff;
  exports["applicativeEff"] = applicativeEff;
  exports["bindEff"] = bindEff;
  exports["monadEff"] = monadEff;
  exports["runPure"] = $foreign.runPure;
})(PS["Control.Monad.Eff"] = PS["Control.Monad.Eff"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  exports.runFn2 = function (fn) {
    return function (a) {
      return function (b) {
        return fn(a, b);
      };
    };
  };

  exports.runFn4 = function (fn) {
    return function (a) {
      return function (b) {
        return function (c) {
          return function (d) {
            return fn(a, b, c, d);
          };
        };
      };
    };
  };
})(PS["Data.Function"] = PS["Data.Function"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["Data.Function"];
  var Prelude = PS["Prelude"];        
  var on = function (f) {
      return function (g) {
          return function (x) {
              return function (y) {
                  return f(g(x))(g(y));
              };
          };
      };
  };
  exports["on"] = on;
  exports["runFn4"] = $foreign.runFn4;
  exports["runFn2"] = $foreign.runFn2;
})(PS["Data.Function"] = PS["Data.Function"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Data_Generic = PS["Data.Generic"];
  var Data_Maybe = PS["Data.Maybe"];
  var Milliseconds = function (x) {
      return x;
  }; 
  var genericMilliseconds = new Data_Generic.Generic(function (v) {
      if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Time.Milliseconds" && v.value1.length === 1)) {
          return Prelude.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Milliseconds))(Data_Generic.fromSpine(Data_Generic.genericNumber)(v.value1[0](Prelude.unit)));
      };
      return Data_Maybe.Nothing.value;
  }, function ($dollarq) {
      return new Data_Generic.SigProd("Data.Time.Milliseconds", [ {
          sigConstructor: "Data.Time.Milliseconds", 
          sigValues: [ function ($dollarq1) {
              return Data_Generic.toSignature(Data_Generic.genericNumber)(Data_Generic.anyProxy);
          } ]
      } ]);
  }, function (v) {
      return new Data_Generic.SProd("Data.Time.Milliseconds", [ function ($dollarq) {
          return Data_Generic.toSpine(Data_Generic.genericNumber)(v);
      } ]);
  });
  exports["Milliseconds"] = Milliseconds;
  exports["genericMilliseconds"] = genericMilliseconds;
})(PS["Data.Time"] = PS["Data.Time"] || {});
(function(exports) {
  /* globals exports */
  "use strict";     

  exports.isNaN = isNaN;        
  exports.encodeURI = encodeURI;
})(PS["Global"] = PS["Global"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["Global"];
  exports["encodeURI"] = $foreign["encodeURI"];
  exports["isNaN"] = $foreign["isNaN"];
})(PS["Global"] = PS["Global"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["Data.Date"];
  var Prelude = PS["Prelude"];
  var Type_Proxy = PS["Type.Proxy"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Data_Enum = PS["Data.Enum"];
  var Data_Function = PS["Data.Function"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Time = PS["Data.Time"];
  var Data_Generic = PS["Data.Generic"];
  var Global = PS["Global"];
  var DateTime = function (x) {
      return x;
  };
  var toEpochMilliseconds = function (v) {
      return $foreign.jsDateMethod("getTime", v);
  }; 
  var now = $foreign.nowImpl(DateTime);
  var fromJSDate = function (d) {
      var $44 = Global["isNaN"]($foreign.jsDateMethod("getTime", d));
      if ($44) {
          return Data_Maybe.Nothing.value;
      };
      if (!$44) {
          return new Data_Maybe.Just(d);
      };
      throw new Error("Failed pattern match at Data.Date line 61, column 3 - line 66, column 1: " + [ $44.constructor.name ]);
  };
  var fromEpochMilliseconds = function ($64) {
      return fromJSDate($foreign.jsDateConstructor($64));
  };
  var genericDate = new Data_Generic.Generic(function (v) {
      if (v instanceof Data_Generic.SProd && (v.value0 === "Date" && v.value1.length === 1)) {
          return Prelude[">>="](Data_Maybe.bindMaybe)(Data_Generic.fromSpine(Data_Time.genericMilliseconds)(v.value1[0](Prelude.unit)))(fromEpochMilliseconds);
      };
      return Data_Maybe.Nothing.value;
  }, function (v) {
      return new Data_Generic.SigProd("Date", [ {
          sigConstructor: "Date", 
          sigValues: [ Prelude["const"](Data_Generic.toSignature(Data_Time.genericMilliseconds)((Type_Proxy["Proxy"]).value)) ]
      } ]);
  }, function (d) {
      return new Data_Generic.SProd("Date", [ Prelude["const"](Data_Generic.toSpine(Data_Time.genericMilliseconds)(toEpochMilliseconds(d))) ]);
  });
  exports["now"] = now;
  exports["toEpochMilliseconds"] = toEpochMilliseconds;
  exports["fromEpochMilliseconds"] = fromEpochMilliseconds;
  exports["fromJSDate"] = fromJSDate;
  exports["genericDate"] = genericDate;
})(PS["Data.Date"] = PS["Data.Date"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  // module Data.Foreign

  // jshint maxparams: 3
  exports.parseJSONImpl = function (left, right, str) {
    try {
      return right(JSON.parse(str));
    } catch (e) {
      return left(e.toString());
    }
  };

  // jshint maxparams: 1
  exports.toForeign = function (value) {
    return value;
  };

  exports.unsafeFromForeign = function (value) {
    return value;
  };

  exports.typeOf = function (value) {
    return typeof value;
  };

  exports.tagOf = function (value) {
    return Object.prototype.toString.call(value).slice(8, -1);
  };

  exports.isNull = function (value) {
    return value === null;
  };

  exports.isUndefined = function (value) {
    return value === undefined;
  };

  exports.isArray = Array.isArray || function (value) {
    return Object.prototype.toString.call(value) === "[object Array]";
  };
})(PS["Data.Foreign"] = PS["Data.Foreign"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  // module Data.Int

  exports.fromNumberImpl = function (just) {
    return function (nothing) {
      return function (n) {
        /* jshint bitwise: false */
        return (n | 0) === n ? just(n) : nothing;
      };
    };
  };
})(PS["Data.Int"] = PS["Data.Int"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["Data.Int"];
  var Prelude = PS["Prelude"];
  var Data_Int_Bits = PS["Data.Int.Bits"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Maybe_Unsafe = PS["Data.Maybe.Unsafe"];
  var $$Math = PS["Math"];                                                                   
  var fromNumber = $foreign.fromNumberImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
  exports["fromNumber"] = fromNumber;
})(PS["Data.Int"] = PS["Data.Int"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["Data.Foreign"];
  var Prelude = PS["Prelude"];
  var Data_Either = PS["Data.Either"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Function = PS["Data.Function"];
  var Data_Int_1 = PS["Data.Int"];
  var Data_Int_1 = PS["Data.Int"];
  var Data_String = PS["Data.String"];        
  var TypeMismatch = (function () {
      function TypeMismatch(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      TypeMismatch.create = function (value0) {
          return function (value1) {
              return new TypeMismatch(value0, value1);
          };
      };
      return TypeMismatch;
  })();
  var ErrorAtIndex = (function () {
      function ErrorAtIndex(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      ErrorAtIndex.create = function (value0) {
          return function (value1) {
              return new ErrorAtIndex(value0, value1);
          };
      };
      return ErrorAtIndex;
  })();
  var ErrorAtProperty = (function () {
      function ErrorAtProperty(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      ErrorAtProperty.create = function (value0) {
          return function (value1) {
              return new ErrorAtProperty(value0, value1);
          };
      };
      return ErrorAtProperty;
  })();
  var JSONError = (function () {
      function JSONError(value0) {
          this.value0 = value0;
      };
      JSONError.create = function (value0) {
          return new JSONError(value0);
      };
      return JSONError;
  })();
  var unsafeReadTagged = function (tag) {
      return function (value) {
          if ($foreign.tagOf(value) === tag) {
              return Prelude.pure(Data_Either.applicativeEither)($foreign.unsafeFromForeign(value));
          };
          return new Data_Either.Left(new TypeMismatch(tag, $foreign.tagOf(value)));
      };
  };
  var showForeignError = new Prelude.Show(function (v) {
      if (v instanceof TypeMismatch) {
          return "Type mismatch: expected " + (v.value0 + (", found " + v.value1));
      };
      if (v instanceof ErrorAtIndex) {
          return "Error at array index " + (Prelude.show(Prelude.showInt)(v.value0) + (": " + Prelude.show(showForeignError)(v.value1)));
      };
      if (v instanceof ErrorAtProperty) {
          return "Error at property " + (Prelude.show(Prelude.showString)(v.value0) + (": " + Prelude.show(showForeignError)(v.value1)));
      };
      if (v instanceof JSONError) {
          return "JSON error: " + v.value0;
      };
      throw new Error("Failed pattern match at Data.Foreign line 54, column 3 - line 55, column 3: " + [ v.constructor.name ]);
  });
  var readString = unsafeReadTagged("String");
  var readNumber = unsafeReadTagged("Number");
  var readInt = function (value) {
      var error = Data_Either.Left.create(new TypeMismatch("Int", $foreign.tagOf(value)));
      var fromNumber = function ($30) {
          return Data_Maybe.maybe(error)(Prelude.pure(Data_Either.applicativeEither))(Data_Int_1.fromNumber($30));
      };
      return Data_Either.either(Prelude["const"](error))(fromNumber)(readNumber(value));
  };
  var readChar = function (value) {
      var error = Data_Either.Left.create(new TypeMismatch("Char", $foreign.tagOf(value)));
      var fromString = function ($31) {
          return Data_Maybe.maybe(error)(Prelude.pure(Data_Either.applicativeEither))(Data_String.toChar($31));
      };
      return Data_Either.either(Prelude["const"](error))(fromString)(readString(value));
  };
  var readBoolean = unsafeReadTagged("Boolean");
  var readArray = function (value) {
      if ($foreign.isArray(value)) {
          return Prelude.pure(Data_Either.applicativeEither)($foreign.unsafeFromForeign(value));
      };
      return new Data_Either.Left(new TypeMismatch("array", $foreign.tagOf(value)));
  };
  var parseJSON = function (json) {
      return $foreign.parseJSONImpl(function ($32) {
          return Data_Either.Left.create(JSONError.create($32));
      }, Data_Either.Right.create, json);
  };
  exports["TypeMismatch"] = TypeMismatch;
  exports["ErrorAtIndex"] = ErrorAtIndex;
  exports["ErrorAtProperty"] = ErrorAtProperty;
  exports["JSONError"] = JSONError;
  exports["readArray"] = readArray;
  exports["readInt"] = readInt;
  exports["readNumber"] = readNumber;
  exports["readBoolean"] = readBoolean;
  exports["readChar"] = readChar;
  exports["readString"] = readString;
  exports["unsafeReadTagged"] = unsafeReadTagged;
  exports["parseJSON"] = parseJSON;
  exports["showForeignError"] = showForeignError;
  exports["isUndefined"] = $foreign.isUndefined;
  exports["isNull"] = $foreign.isNull;
  exports["typeOf"] = $foreign.typeOf;
  exports["toForeign"] = $foreign.toForeign;
})(PS["Data.Foreign"] = PS["Data.Foreign"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  // module Data.Foreign.Index

  // jshint maxparams: 4
  exports.unsafeReadPropImpl = function (f, s, key, value) {
    return value == null ? f : s(value[key]);
  };

  // jshint maxparams: 2
  exports.unsafeHasOwnProperty = function (prop, value) {
    return Object.prototype.hasOwnProperty.call(value, prop);
  };

  exports.unsafeHasProperty = function (prop, value) {
    return prop in value;
  };
})(PS["Data.Foreign.Index"] = PS["Data.Foreign.Index"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["Data.Foreign.Index"];
  var Prelude = PS["Prelude"];
  var Data_Either = PS["Data.Either"];
  var Data_Foreign = PS["Data.Foreign"];
  var Data_Function = PS["Data.Function"];
  var Data_Int = PS["Data.Int"];        
  var Index = function (errorAt, hasOwnProperty, hasProperty, ix) {
      this.errorAt = errorAt;
      this.hasOwnProperty = hasOwnProperty;
      this.hasProperty = hasProperty;
      this.ix = ix;
  };
  var unsafeReadProp = function (k) {
      return function (value) {
          return $foreign.unsafeReadPropImpl(new Data_Either.Left(new Data_Foreign.TypeMismatch("object", Data_Foreign.typeOf(value))), Prelude.pure(Data_Either.applicativeEither), k, value);
      };
  };
  var prop = unsafeReadProp;
  var ix = function (dict) {
      return dict.ix;
  };
  var $bang = function (dictIndex) {
      return ix(dictIndex);
  };                         
  var hasPropertyImpl = function (v) {
      return function (value) {
          if (Data_Foreign.isNull(value)) {
              return false;
          };
          if (Data_Foreign.isUndefined(value)) {
              return false;
          };
          if (Data_Foreign.typeOf(value) === "object" || Data_Foreign.typeOf(value) === "function") {
              return $foreign.unsafeHasProperty(v, value);
          };
          return false;
      };
  };
  var hasProperty = function (dict) {
      return dict.hasProperty;
  };
  var hasOwnPropertyImpl = function (v) {
      return function (value) {
          if (Data_Foreign.isNull(value)) {
              return false;
          };
          if (Data_Foreign.isUndefined(value)) {
              return false;
          };
          if (Data_Foreign.typeOf(value) === "object" || Data_Foreign.typeOf(value) === "function") {
              return $foreign.unsafeHasOwnProperty(v, value);
          };
          return false;
      };
  };                                                                                                                   
  var indexString = new Index(Data_Foreign.ErrorAtProperty.create, hasOwnPropertyImpl, hasPropertyImpl, Prelude.flip(prop));
  var hasOwnProperty = function (dict) {
      return dict.hasOwnProperty;
  };
  var errorAt = function (dict) {
      return dict.errorAt;
  };
  exports["Index"] = Index;
  exports["errorAt"] = errorAt;
  exports["hasOwnProperty"] = hasOwnProperty;
  exports["hasProperty"] = hasProperty;
  exports["!"] = $bang;
  exports["ix"] = ix;
  exports["prop"] = prop;
  exports["indexString"] = indexString;
})(PS["Data.Foreign.Index"] = PS["Data.Foreign.Index"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Data_Array = PS["Data.Array"];
  var Data_Either = PS["Data.Either"];
  var Data_Foreign = PS["Data.Foreign"];
  var Data_Foreign_Index = PS["Data.Foreign.Index"];
  var Data_Foreign_Null = PS["Data.Foreign.Null"];
  var Data_Foreign_NullOrUndefined = PS["Data.Foreign.NullOrUndefined"];
  var Data_Foreign_Undefined = PS["Data.Foreign.Undefined"];
  var Data_Int = PS["Data.Int"];
  var Data_Traversable = PS["Data.Traversable"];        
  var IsForeign = function (read) {
      this.read = read;
  };
  var stringIsForeign = new IsForeign(Data_Foreign.readString);
  var read = function (dict) {
      return dict.read;
  };
  var readWith = function (dictIsForeign) {
      return function (f) {
          return function (value) {
              return Data_Either.either(function ($8) {
                  return Data_Either.Left.create(f($8));
              })(Data_Either.Right.create)(read(dictIsForeign)(value));
          };
      };
  };
  var readProp = function (dictIsForeign) {
      return function (dictIndex) {
          return function (prop) {
              return function (value) {
                  return Prelude[">>="](Data_Either.bindEither)(Data_Foreign_Index["!"](dictIndex)(value)(prop))(readWith(dictIsForeign)(Data_Foreign_Index.errorAt(dictIndex)(prop)));
              };
          };
      };
  };
  exports["IsForeign"] = IsForeign;
  exports["readProp"] = readProp;
  exports["readWith"] = readWith;
  exports["read"] = read;
  exports["stringIsForeign"] = stringIsForeign;
})(PS["Data.Foreign.Class"] = PS["Data.Foreign.Class"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  exports._copyEff = function (m) {
    return function () {
      var r = {};
      for (var k in m) {
        if (m.hasOwnProperty(k)) {
          r[k] = m[k];
        }
      }
      return r;
    };
  };

  exports.empty = {};

  exports.runST = function (f) {
    return f;
  };

  // jshint maxparams: 1
  exports._foldM = function (bind) {
    return function (f) {
      return function (mz) {
        return function (m) {
          function g (k) {
            return function (z) {
              return f(z)(k)(m[k]);
            };
          }
          for (var k in m) {
            if (m.hasOwnProperty(k)) {
              mz = bind(mz)(g(k));
            }
          }
          return mz;
        };
      };
    };
  };

  // jshint maxparams: 4
  exports._lookup = function (no, yes, k, m) {
    return k in m ? yes(m[k]) : no;
  };

  function _collect (f) {
    return function (m) {
      var r = [];
      for (var k in m) {
        if (m.hasOwnProperty(k)) {
          r.push(f(k)(m[k]));
        }
      }
      return r;
    };
  }
})(PS["Data.StrMap"] = PS["Data.StrMap"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Control_Alt = PS["Control.Alt"];
  var Control_Alternative = PS["Control.Alternative"];
  var Control_Lazy = PS["Control.Lazy"];
  var Control_MonadPlus = PS["Control.MonadPlus"];
  var Control_Plus = PS["Control.Plus"];
  var Data_Foldable = PS["Data.Foldable"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_Traversable = PS["Data.Traversable"];
  var Data_Tuple = PS["Data.Tuple"];
  var Data_Unfoldable = PS["Data.Unfoldable"];        
  var Nil = (function () {
      function Nil() {

      };
      Nil.value = new Nil();
      return Nil;
  })();
  var Cons = (function () {
      function Cons(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      Cons.create = function (value0) {
          return function (value1) {
              return new Cons(value0, value1);
          };
      };
      return Cons;
  })();
  var singleton = function (a) {
      return new Cons(a, Nil.value);
  };
  var fromFoldable = function (dictFoldable) {
      return Data_Foldable.foldr(dictFoldable)(Cons.create)(Nil.value);
  };
  var toList = function (dictFoldable) {
      return fromFoldable(dictFoldable);
  };
  var foldableList = new Data_Foldable.Foldable(function (dictMonoid) {
      return function (f) {
          return Data_Foldable.foldl(foldableList)(function (acc) {
              return function ($374) {
                  return Prelude.append(dictMonoid["__superclass_Prelude.Semigroup_0"]())(acc)(f($374));
              };
          })(Data_Monoid.mempty(dictMonoid));
      };
  }, (function () {
      var go = function (__copy_v) {
          return function (__copy_b) {
              return function (__copy_v1) {
                  var v = __copy_v;
                  var b = __copy_b;
                  var v1 = __copy_v1;
                  tco: while (true) {
                      if (v1 instanceof Nil) {
                          return b;
                      };
                      if (v1 instanceof Cons) {
                          var __tco_v = v;
                          var __tco_b = v(b)(v1.value0);
                          var __tco_v1 = v1.value1;
                          v = __tco_v;
                          b = __tco_b;
                          v1 = __tco_v1;
                          continue tco;
                      };
                      throw new Error("Failed pattern match at Data.List line 767, column 3 - line 771, column 3: " + [ v.constructor.name, b.constructor.name, v1.constructor.name ]);
                  };
              };
          };
      };
      return go;
  })(), function (v) {
      return function (b) {
          return function (v1) {
              if (v1 instanceof Nil) {
                  return b;
              };
              if (v1 instanceof Cons) {
                  return v(v1.value0)(Data_Foldable.foldr(foldableList)(v)(b)(v1.value1));
              };
              throw new Error("Failed pattern match at Data.List line 765, column 3 - line 766, column 3: " + [ v.constructor.name, b.constructor.name, v1.constructor.name ]);
          };
      };
  });
  exports["Nil"] = Nil;
  exports["Cons"] = Cons;
  exports["toList"] = toList;
  exports["singleton"] = singleton;
  exports["fromFoldable"] = fromFoldable;
  exports["foldableList"] = foldableList;
})(PS["Data.List"] = PS["Data.List"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  // module Data.StrMap.ST

  exports["new"] = function () {
    return {};
  };

  exports.poke = function (m) {
    return function (k) {
      return function (v) {
        return function () {
          m[k] = v;
          return m;
        };
      };
    };
  };
})(PS["Data.StrMap.ST"] = PS["Data.StrMap.ST"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["Data.StrMap.ST"];
  var Prelude = PS["Prelude"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Control_Monad_ST = PS["Control.Monad.ST"];
  var Data_Maybe = PS["Data.Maybe"];
  exports["poke"] = $foreign.poke;
  exports["new"] = $foreign["new"];
})(PS["Data.StrMap.ST"] = PS["Data.StrMap.ST"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["Data.StrMap"];
  var Prelude = PS["Prelude"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Data_Foldable = PS["Data.Foldable"];
  var Data_Function = PS["Data.Function"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_Traversable = PS["Data.Traversable"];
  var Data_Tuple = PS["Data.Tuple"];
  var Data_List = PS["Data.List"];
  var Control_Monad_ST = PS["Control.Monad.ST"];
  var Data_StrMap_ST = PS["Data.StrMap.ST"];
  var thawST = $foreign._copyEff;
  var pureST = function (f) {
      return Control_Monad_Eff.runPure($foreign.runST(f));
  };
  var mutate = function (f) {
      return function (m) {
          return pureST(function __do() {
              var v = thawST(m)();
              f(v)();
              return v;
          });
      };
  };                                                                                 
  var lookup = Data_Function.runFn4($foreign._lookup)(Data_Maybe.Nothing.value)(Data_Maybe.Just.create);
  var insert = function (k) {
      return function (v) {
          return mutate(function (s) {
              return Data_StrMap_ST.poke(s)(k)(v);
          });
      };
  };                                                          
  var fromFoldable = function (dictFoldable) {
      return function (l) {
          return pureST(function __do() {
              var v = Data_StrMap_ST["new"]();
              Data_Foldable.for_(Control_Monad_Eff.applicativeEff)(dictFoldable)(l)(function (v1) {
                  return Data_StrMap_ST.poke(v)(v1.value0)(v1.value1);
              })();
              return v;
          });
      };
  };
  var fromList = fromFoldable(Data_List.foldableList);
  var foldM = function (dictMonad) {
      return function (f) {
          return function (z) {
              return $foreign._foldM(Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]()))(f)(Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(z));
          };
      };
  };
  var semigroupStrMap = function (dictSemigroup) {
      return new Prelude.Semigroup(function (m1) {
          return function (m2) {
              return mutate(function (s1) {
                  return foldM(Control_Monad_Eff.monadEff)(function (s2) {
                      return function (k) {
                          return function (v2) {
                              return Data_StrMap_ST.poke(s2)(k)($foreign._lookup(v2, function (v1) {
                                  return Prelude["<>"](dictSemigroup)(v1)(v2);
                              }, k, m2));
                          };
                      };
                  })(s1)(m1);
              })(m2);
          };
      });
  };
  var monoidStrMap = function (dictSemigroup) {
      return new Data_Monoid.Monoid(function () {
          return semigroupStrMap(dictSemigroup);
      }, $foreign.empty);
  };
  exports["thawST"] = thawST;
  exports["foldM"] = foldM;
  exports["fromList"] = fromList;
  exports["fromFoldable"] = fromFoldable;
  exports["lookup"] = lookup;
  exports["insert"] = insert;
  exports["semigroupStrMap"] = semigroupStrMap;
  exports["monoidStrMap"] = monoidStrMap;
  exports["empty"] = $foreign.empty;
})(PS["Data.StrMap"] = PS["Data.StrMap"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["App.Model.StrMap"];
  var Prelude = PS["Prelude"];
  var Data_StrMap = PS["Data.StrMap"];
  var Data_Tuple = PS["Data.Tuple"];
  var Data_Foldable = PS["Data.Foldable"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_Array = PS["Data.Array"];                       
  var fromArray = Data_Foldable.foldl(Data_Foldable.foldableArray)(function (acc) {
      return function (v) {
          return Data_StrMap.insert(v.value0)(v.value1)(acc);
      };
  })(Data_StrMap.empty);
  exports["fromArray"] = fromArray;
})(PS["App.Model.StrMap"] = PS["App.Model.StrMap"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];        
  var Profunctor = function (dimap) {
      this.dimap = dimap;
  };
  var profunctorFn = new Profunctor(function (a2b) {
      return function (c2d) {
          return function (b2c) {
              return function ($4) {
                  return c2d(b2c(a2b($4)));
              };
          };
      };
  });
  var dimap = function (dict) {
      return dict.dimap;
  };
  var rmap = function (dictProfunctor) {
      return function (b2c) {
          return dimap(dictProfunctor)(Prelude.id(Prelude.categoryFn))(b2c);
      };
  };
  exports["Profunctor"] = Profunctor;
  exports["rmap"] = rmap;
  exports["dimap"] = dimap;
  exports["profunctorFn"] = profunctorFn;
})(PS["Data.Profunctor"] = PS["Data.Profunctor"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Data_Profunctor = PS["Data.Profunctor"];
  var Data_Tuple = PS["Data.Tuple"];        
  var Strong = function (__superclass_Data$dotProfunctor$dotProfunctor_0, first, second) {
      this["__superclass_Data.Profunctor.Profunctor_0"] = __superclass_Data$dotProfunctor$dotProfunctor_0;
      this.first = first;
      this.second = second;
  };
  var strongFn = new Strong(function () {
      return Data_Profunctor.profunctorFn;
  }, function (a2b) {
      return function (v) {
          return new Data_Tuple.Tuple(a2b(v.value0), v.value1);
      };
  }, Prelude["<$>"](Data_Tuple.functorTuple));
  var second = function (dict) {
      return dict.second;
  };
  var first = function (dict) {
      return dict.first;
  };
  exports["Strong"] = Strong;
  exports["second"] = second;
  exports["first"] = first;
  exports["strongFn"] = strongFn;
})(PS["Data.Profunctor.Strong"] = PS["Data.Profunctor.Strong"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Data_Profunctor = PS["Data.Profunctor"];
  var Data_Profunctor_Strong = PS["Data.Profunctor.Strong"];
  var Data_Tuple = PS["Data.Tuple"];
  var Data_Lens_Internal_Shop = PS["Data.Lens.Internal.Shop"];
  var Data_Lens_Types = PS["Data.Lens.Types"];
  var lens$prime = function (to) {
      return function (dictStrong) {
          return function (pab) {
              return Data_Profunctor.dimap(dictStrong["__superclass_Data.Profunctor.Profunctor_0"]())(to)(function (v) {
                  return v.value1(v.value0);
              })(Data_Profunctor_Strong.first(dictStrong)(pab));
          };
      };
  };
  var lens = function (get) {
      return function (set) {
          return function (dictStrong) {
              return lens$prime(function (s) {
                  return new Data_Tuple.Tuple(get(s), function (b) {
                      return set(s)(b);
                  });
              })(dictStrong);
          };
      };
  };
  exports["lens"] = lens;
})(PS["Data.Lens.Lens"] = PS["Data.Lens.Lens"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var SQL = PS["SQL"];
  var Data_Foreign_Class = PS["Data.Foreign.Class"];
  var Data_Generic = PS["Data.Generic"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Lens = PS["Data.Lens"];
  var Data_Tuple = PS["Data.Tuple"];
  var Data_Array = PS["Data.Array"];
  var App_Model_StrMap = PS["App.Model.StrMap"];
  var Data_Either = PS["Data.Either"];
  var Data_Foreign_Index = PS["Data.Foreign.Index"];
  var Data_Lens_Lens = PS["Data.Lens.Lens"];        
  var Photobooth = (function () {
      function Photobooth(value0) {
          this.value0 = value0;
      };
      Photobooth.create = function (value0) {
          return new Photobooth(value0);
      };
      return Photobooth;
  })();
  var sortPhotobooths = Data_Array.sortBy(function (v) {
      return function (v1) {
          return Prelude.compare(Prelude.ordInt)(Data_Maybe.maybe(999999)(Prelude.id(Prelude.categoryFn))(v1.value0.id))(Data_Maybe.maybe(999999)(Prelude.id(Prelude.categoryFn))(v.value0.id));
      };
  });
  var genericPhotobooth = new Data_Generic.Generic(function (v) {
      if (v instanceof Data_Generic.SProd && (v.value0 === "App.Model.Photobooth.Photobooth" && v.value1.length === 1)) {
          return Prelude.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Photobooth.create))((function (r) {
              if (r instanceof Data_Generic.SRecord && r.value0.length === 4) {
                  return Prelude.apply(Data_Maybe.applyMaybe)(Prelude.apply(Data_Maybe.applyMaybe)(Prelude.apply(Data_Maybe.applyMaybe)(Prelude.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(function (alias1) {
                      return function (computername1) {
                          return function (defaultprofile1) {
                              return function (id1) {
                                  return {
                                      alias: alias1, 
                                      computername: computername1, 
                                      defaultprofile: defaultprofile1, 
                                      id: id1
                                  };
                              };
                          };
                      };
                  }))(Data_Generic.fromSpine(Data_Generic.genericString)((r.value0[0]).recValue(Prelude.unit))))(Data_Generic.fromSpine(Data_Generic.genericString)((r.value0[1]).recValue(Prelude.unit))))(Data_Generic.fromSpine(Data_Generic.genericString)((r.value0[2]).recValue(Prelude.unit))))(Data_Generic.fromSpine(Data_Generic.genericMaybe(Data_Generic.genericInt))((r.value0[3]).recValue(Prelude.unit)));
              };
              return Data_Maybe.Nothing.value;
          })(v.value1[0](Prelude.unit)));
      };
      return Data_Maybe.Nothing.value;
  }, function ($dollarq) {
      return new Data_Generic.SigProd("App.Model.Photobooth.Photobooth", [ {
          sigConstructor: "App.Model.Photobooth.Photobooth", 
          sigValues: [ function ($dollarq1) {
              return new Data_Generic.SigRecord([ {
                  recLabel: "alias", 
                  recValue: function ($dollarq2) {
                      return Data_Generic.toSignature(Data_Generic.genericString)(Data_Generic.anyProxy);
                  }
              }, {
                  recLabel: "computername", 
                  recValue: function ($dollarq2) {
                      return Data_Generic.toSignature(Data_Generic.genericString)(Data_Generic.anyProxy);
                  }
              }, {
                  recLabel: "defaultprofile", 
                  recValue: function ($dollarq2) {
                      return Data_Generic.toSignature(Data_Generic.genericString)(Data_Generic.anyProxy);
                  }
              }, {
                  recLabel: "id", 
                  recValue: function ($dollarq2) {
                      return Data_Generic.toSignature(Data_Generic.genericMaybe(Data_Generic.genericInt))(Data_Generic.anyProxy);
                  }
              } ]);
          } ]
      } ]);
  }, function (v) {
      return new Data_Generic.SProd("App.Model.Photobooth.Photobooth", [ function ($dollarq) {
          return new Data_Generic.SRecord([ {
              recLabel: "alias", 
              recValue: function ($dollarq1) {
                  return Data_Generic.toSpine(Data_Generic.genericString)(v.value0.alias);
              }
          }, {
              recLabel: "computername", 
              recValue: function ($dollarq1) {
                  return Data_Generic.toSpine(Data_Generic.genericString)(v.value0.computername);
              }
          }, {
              recLabel: "defaultprofile", 
              recValue: function ($dollarq1) {
                  return Data_Generic.toSpine(Data_Generic.genericString)(v.value0.defaultprofile);
              }
          }, {
              recLabel: "id", 
              recValue: function ($dollarq1) {
                  return Data_Generic.toSpine(Data_Generic.genericMaybe(Data_Generic.genericInt))(v.value0.id);
              }
          } ]);
      } ]);
  });                                                                    
  var _Photobooth = Data_Lens_Lens.lens(function (v) {
      return v.value0;
  })(function (v) {
      return function (a) {
          return new Photobooth(a);
      };
  });
  exports["Photobooth"] = Photobooth;
  exports["_Photobooth"] = _Photobooth;
  exports["sortPhotobooths"] = sortPhotobooths;
  exports["genericPhotobooth"] = genericPhotobooth;
})(PS["App.Model.Photobooth"] = PS["App.Model.Photobooth"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var SQL = PS["SQL"];
  var Data_Foreign_Class = PS["Data.Foreign.Class"];
  var Data_Generic = PS["Data.Generic"];
  var Data_Tuple = PS["Data.Tuple"];
  var App_Model_StrMap = PS["App.Model.StrMap"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Either = PS["Data.Either"];
  var Data_Foreign_Index = PS["Data.Foreign.Index"];        
  var SavedFile = (function () {
      function SavedFile(value0) {
          this.value0 = value0;
      };
      SavedFile.create = function (value0) {
          return new SavedFile(value0);
      };
      return SavedFile;
  })();
  var genericSavedFile = new Data_Generic.Generic(function (v) {
      if (v instanceof Data_Generic.SProd && (v.value0 === "App.Model.SavedFile.SavedFile" && v.value1.length === 1)) {
          return Prelude.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(SavedFile.create))((function (r) {
              if (r instanceof Data_Generic.SRecord && r.value0.length === 3) {
                  return Prelude.apply(Data_Maybe.applyMaybe)(Prelude.apply(Data_Maybe.applyMaybe)(Prelude.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(function (eventId1) {
                      return function (id1) {
                          return function (name1) {
                              return {
                                  eventId: eventId1, 
                                  id: id1, 
                                  name: name1
                              };
                          };
                      };
                  }))(Data_Generic.fromSpine(Data_Generic.genericInt)((r.value0[0]).recValue(Prelude.unit))))(Data_Generic.fromSpine(Data_Generic.genericInt)((r.value0[1]).recValue(Prelude.unit))))(Data_Generic.fromSpine(Data_Generic.genericString)((r.value0[2]).recValue(Prelude.unit)));
              };
              return Data_Maybe.Nothing.value;
          })(v.value1[0](Prelude.unit)));
      };
      return Data_Maybe.Nothing.value;
  }, function ($dollarq) {
      return new Data_Generic.SigProd("App.Model.SavedFile.SavedFile", [ {
          sigConstructor: "App.Model.SavedFile.SavedFile", 
          sigValues: [ function ($dollarq1) {
              return new Data_Generic.SigRecord([ {
                  recLabel: "eventId", 
                  recValue: function ($dollarq2) {
                      return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
                  }
              }, {
                  recLabel: "id", 
                  recValue: function ($dollarq2) {
                      return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
                  }
              }, {
                  recLabel: "name", 
                  recValue: function ($dollarq2) {
                      return Data_Generic.toSignature(Data_Generic.genericString)(Data_Generic.anyProxy);
                  }
              } ]);
          } ]
      } ]);
  }, function (v) {
      return new Data_Generic.SProd("App.Model.SavedFile.SavedFile", [ function ($dollarq) {
          return new Data_Generic.SRecord([ {
              recLabel: "eventId", 
              recValue: function ($dollarq1) {
                  return Data_Generic.toSpine(Data_Generic.genericInt)(v.value0.eventId);
              }
          }, {
              recLabel: "id", 
              recValue: function ($dollarq1) {
                  return Data_Generic.toSpine(Data_Generic.genericInt)(v.value0.id);
              }
          }, {
              recLabel: "name", 
              recValue: function ($dollarq1) {
                  return Data_Generic.toSpine(Data_Generic.genericString)(v.value0.name);
              }
          } ]);
      } ]);
  });
  exports["SavedFile"] = SavedFile;
  exports["genericSavedFile"] = genericSavedFile;
})(PS["App.Model.SavedFile"] = PS["App.Model.SavedFile"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var SQL = PS["SQL"];
  var Data_Foreign = PS["Data.Foreign"];
  var Data_Foreign_Class = PS["Data.Foreign.Class"];
  var Data_Generic = PS["Data.Generic"];
  var Data_Lens = PS["Data.Lens"];
  var Data_Tuple = PS["Data.Tuple"];
  var Data_Date = PS["Data.Date"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Either = PS["Data.Either"];
  var Data_Array = PS["Data.Array"];
  var App_Model_StrMap = PS["App.Model.StrMap"];
  var App_Model_SavedFile = PS["App.Model.SavedFile"];
  var Data_Foreign_Index = PS["Data.Foreign.Index"];
  var Data_Lens_Lens = PS["Data.Lens.Lens"];
  var Event = (function () {
      function Event(value0) {
          this.value0 = value0;
      };
      Event.create = function (value0) {
          return new Event(value0);
      };
      return Event;
  })();
  var sortEvents = Data_Array.sortBy(function (v) {
      return function (v1) {
          return Prelude.compare(Prelude.ordInt)(Data_Maybe.maybe(999999)(Prelude.id(Prelude.categoryFn))(v1.value0.id))(Data_Maybe.maybe(999999)(Prelude.id(Prelude.categoryFn))(v.value0.id));
      };
  });
  var genericEvent = new Data_Generic.Generic(function (v) {
      if (v instanceof Data_Generic.SProd && (v.value0 === "App.Model.Event.Event" && v.value1.length === 1)) {
          return Prelude.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Event.create))((function (r) {
              if (r instanceof Data_Generic.SRecord && r.value0.length === 7) {
                  return Prelude.apply(Data_Maybe.applyMaybe)(Prelude.apply(Data_Maybe.applyMaybe)(Prelude.apply(Data_Maybe.applyMaybe)(Prelude.apply(Data_Maybe.applyMaybe)(Prelude.apply(Data_Maybe.applyMaybe)(Prelude.apply(Data_Maybe.applyMaybe)(Prelude.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(function (computername1) {
                      return function (datefrom1) {
                          return function (dateuntil1) {
                              return function (files1) {
                                  return function (id1) {
                                      return function (name1) {
                                          return function (profile1) {
                                              return {
                                                  computername: computername1, 
                                                  datefrom: datefrom1, 
                                                  dateuntil: dateuntil1, 
                                                  files: files1, 
                                                  id: id1, 
                                                  name: name1, 
                                                  profile: profile1
                                              };
                                          };
                                      };
                                  };
                              };
                          };
                      };
                  }))(Data_Generic.fromSpine(Data_Generic.genericString)((r.value0[0]).recValue(Prelude.unit))))(Data_Generic.fromSpine(Data_Date.genericDate)((r.value0[1]).recValue(Prelude.unit))))(Data_Generic.fromSpine(Data_Date.genericDate)((r.value0[2]).recValue(Prelude.unit))))(Data_Generic.fromSpine(Data_Generic.genericArray(App_Model_SavedFile.genericSavedFile))((r.value0[3]).recValue(Prelude.unit))))(Data_Generic.fromSpine(Data_Generic.genericMaybe(Data_Generic.genericInt))((r.value0[4]).recValue(Prelude.unit))))(Data_Generic.fromSpine(Data_Generic.genericString)((r.value0[5]).recValue(Prelude.unit))))(Data_Generic.fromSpine(Data_Generic.genericString)((r.value0[6]).recValue(Prelude.unit)));
              };
              return Data_Maybe.Nothing.value;
          })(v.value1[0](Prelude.unit)));
      };
      return Data_Maybe.Nothing.value;
  }, function ($dollarq) {
      return new Data_Generic.SigProd("App.Model.Event.Event", [ {
          sigConstructor: "App.Model.Event.Event", 
          sigValues: [ function ($dollarq1) {
              return new Data_Generic.SigRecord([ {
                  recLabel: "computername", 
                  recValue: function ($dollarq2) {
                      return Data_Generic.toSignature(Data_Generic.genericString)(Data_Generic.anyProxy);
                  }
              }, {
                  recLabel: "datefrom", 
                  recValue: function ($dollarq2) {
                      return Data_Generic.toSignature(Data_Date.genericDate)(Data_Generic.anyProxy);
                  }
              }, {
                  recLabel: "dateuntil", 
                  recValue: function ($dollarq2) {
                      return Data_Generic.toSignature(Data_Date.genericDate)(Data_Generic.anyProxy);
                  }
              }, {
                  recLabel: "files", 
                  recValue: function ($dollarq2) {
                      return Data_Generic.toSignature(Data_Generic.genericArray(App_Model_SavedFile.genericSavedFile))(Data_Generic.anyProxy);
                  }
              }, {
                  recLabel: "id", 
                  recValue: function ($dollarq2) {
                      return Data_Generic.toSignature(Data_Generic.genericMaybe(Data_Generic.genericInt))(Data_Generic.anyProxy);
                  }
              }, {
                  recLabel: "name", 
                  recValue: function ($dollarq2) {
                      return Data_Generic.toSignature(Data_Generic.genericString)(Data_Generic.anyProxy);
                  }
              }, {
                  recLabel: "profile", 
                  recValue: function ($dollarq2) {
                      return Data_Generic.toSignature(Data_Generic.genericString)(Data_Generic.anyProxy);
                  }
              } ]);
          } ]
      } ]);
  }, function (v) {
      return new Data_Generic.SProd("App.Model.Event.Event", [ function ($dollarq) {
          return new Data_Generic.SRecord([ {
              recLabel: "computername", 
              recValue: function ($dollarq1) {
                  return Data_Generic.toSpine(Data_Generic.genericString)(v.value0.computername);
              }
          }, {
              recLabel: "datefrom", 
              recValue: function ($dollarq1) {
                  return Data_Generic.toSpine(Data_Date.genericDate)(v.value0.datefrom);
              }
          }, {
              recLabel: "dateuntil", 
              recValue: function ($dollarq1) {
                  return Data_Generic.toSpine(Data_Date.genericDate)(v.value0.dateuntil);
              }
          }, {
              recLabel: "files", 
              recValue: function ($dollarq1) {
                  return Data_Generic.toSpine(Data_Generic.genericArray(App_Model_SavedFile.genericSavedFile))(v.value0.files);
              }
          }, {
              recLabel: "id", 
              recValue: function ($dollarq1) {
                  return Data_Generic.toSpine(Data_Generic.genericMaybe(Data_Generic.genericInt))(v.value0.id);
              }
          }, {
              recLabel: "name", 
              recValue: function ($dollarq1) {
                  return Data_Generic.toSpine(Data_Generic.genericString)(v.value0.name);
              }
          }, {
              recLabel: "profile", 
              recValue: function ($dollarq1) {
                  return Data_Generic.toSpine(Data_Generic.genericString)(v.value0.profile);
              }
          } ]);
      } ]);
  });
  var _Event = Data_Lens_Lens.lens(function (v) {
      return v.value0;
  })(function (v) {
      return function (a) {
          return new Event(a);
      };
  });
  exports["Event"] = Event;
  exports["_Event"] = _Event;
  exports["sortEvents"] = sortEvents;
  exports["genericEvent"] = genericEvent;
})(PS["App.Model.Event"] = PS["App.Model.Event"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var SQL = PS["SQL"];
  var Data_Foreign = PS["Data.Foreign"];
  var Data_Foreign_Class = PS["Data.Foreign.Class"];
  var Data_Generic = PS["Data.Generic"];
  var Data_Lens = PS["Data.Lens"];
  var Data_Tuple = PS["Data.Tuple"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Date = PS["Data.Date"];
  var App_Model_StrMap = PS["App.Model.StrMap"];
  var Data_Either = PS["Data.Either"];
  var Data_Foreign_Index = PS["Data.Foreign.Index"];
  var Data_Lens_Lens = PS["Data.Lens.Lens"];        
  var MonthlyStatistic = (function () {
      function MonthlyStatistic(value0) {
          this.value0 = value0;
      };
      MonthlyStatistic.create = function (value0) {
          return new MonthlyStatistic(value0);
      };
      return MonthlyStatistic;
  })();
  var EventStatistic = (function () {
      function EventStatistic(value0) {
          this.value0 = value0;
      };
      EventStatistic.create = function (value0) {
          return new EventStatistic(value0);
      };
      return EventStatistic;
  })();
  var AllStatistics = (function () {
      function AllStatistics(value0) {
          this.value0 = value0;
      };
      AllStatistics.create = function (value0) {
          return new AllStatistics(value0);
      };
      return AllStatistics;
  })();
  var getMonthText = function (v) {
      if (v === 1) {
          return "Januari";
      };
      if (v === 2) {
          return "Februari";
      };
      if (v === 3) {
          return "Maart";
      };
      if (v === 4) {
          return "April";
      };
      if (v === 5) {
          return "Mei";
      };
      if (v === 6) {
          return "Juni";
      };
      if (v === 7) {
          return "Juli";
      };
      if (v === 8) {
          return "Augustus";
      };
      if (v === 9) {
          return "September";
      };
      if (v === 10) {
          return "October";
      };
      if (v === 11) {
          return "November";
      };
      if (v === 12) {
          return "December";
      };
      return "Foute maand";
  };
  var genericMonthlyStatistic = new Data_Generic.Generic(function (v) {
      if (v instanceof Data_Generic.SProd && (v.value0 === "App.Model.Statistic.MonthlyStatistic" && v.value1.length === 1)) {
          return Prelude.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(MonthlyStatistic.create))((function (r) {
              if (r instanceof Data_Generic.SRecord && r.value0.length === 4) {
                  return Prelude.apply(Data_Maybe.applyMaybe)(Prelude.apply(Data_Maybe.applyMaybe)(Prelude.apply(Data_Maybe.applyMaybe)(Prelude.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(function (computername1) {
                      return function (month1) {
                          return function (pictures1) {
                              return function (prints1) {
                                  return {
                                      computername: computername1, 
                                      month: month1, 
                                      pictures: pictures1, 
                                      prints: prints1
                                  };
                              };
                          };
                      };
                  }))(Data_Generic.fromSpine(Data_Generic.genericString)((r.value0[0]).recValue(Prelude.unit))))(Data_Generic.fromSpine(Data_Generic.genericInt)((r.value0[1]).recValue(Prelude.unit))))(Data_Generic.fromSpine(Data_Generic.genericInt)((r.value0[2]).recValue(Prelude.unit))))(Data_Generic.fromSpine(Data_Generic.genericInt)((r.value0[3]).recValue(Prelude.unit)));
              };
              return Data_Maybe.Nothing.value;
          })(v.value1[0](Prelude.unit)));
      };
      return Data_Maybe.Nothing.value;
  }, function ($dollarq) {
      return new Data_Generic.SigProd("App.Model.Statistic.MonthlyStatistic", [ {
          sigConstructor: "App.Model.Statistic.MonthlyStatistic", 
          sigValues: [ function ($dollarq1) {
              return new Data_Generic.SigRecord([ {
                  recLabel: "computername", 
                  recValue: function ($dollarq2) {
                      return Data_Generic.toSignature(Data_Generic.genericString)(Data_Generic.anyProxy);
                  }
              }, {
                  recLabel: "month", 
                  recValue: function ($dollarq2) {
                      return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
                  }
              }, {
                  recLabel: "pictures", 
                  recValue: function ($dollarq2) {
                      return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
                  }
              }, {
                  recLabel: "prints", 
                  recValue: function ($dollarq2) {
                      return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
                  }
              } ]);
          } ]
      } ]);
  }, function (v) {
      return new Data_Generic.SProd("App.Model.Statistic.MonthlyStatistic", [ function ($dollarq) {
          return new Data_Generic.SRecord([ {
              recLabel: "computername", 
              recValue: function ($dollarq1) {
                  return Data_Generic.toSpine(Data_Generic.genericString)(v.value0.computername);
              }
          }, {
              recLabel: "month", 
              recValue: function ($dollarq1) {
                  return Data_Generic.toSpine(Data_Generic.genericInt)(v.value0.month);
              }
          }, {
              recLabel: "pictures", 
              recValue: function ($dollarq1) {
                  return Data_Generic.toSpine(Data_Generic.genericInt)(v.value0.pictures);
              }
          }, {
              recLabel: "prints", 
              recValue: function ($dollarq1) {
                  return Data_Generic.toSpine(Data_Generic.genericInt)(v.value0.prints);
              }
          } ]);
      } ]);
  });                                                                                      
  var genericEventStatistic = new Data_Generic.Generic(function (v) {
      if (v instanceof Data_Generic.SProd && (v.value0 === "App.Model.Statistic.EventStatistic" && v.value1.length === 1)) {
          return Prelude.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(EventStatistic.create))((function (r) {
              if (r instanceof Data_Generic.SRecord && r.value0.length === 4) {
                  return Prelude.apply(Data_Maybe.applyMaybe)(Prelude.apply(Data_Maybe.applyMaybe)(Prelude.apply(Data_Maybe.applyMaybe)(Prelude.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(function (computername1) {
                      return function (eventId1) {
                          return function (pictures1) {
                              return function (prints1) {
                                  return {
                                      computername: computername1, 
                                      eventId: eventId1, 
                                      pictures: pictures1, 
                                      prints: prints1
                                  };
                              };
                          };
                      };
                  }))(Data_Generic.fromSpine(Data_Generic.genericString)((r.value0[0]).recValue(Prelude.unit))))(Data_Generic.fromSpine(Data_Generic.genericInt)((r.value0[1]).recValue(Prelude.unit))))(Data_Generic.fromSpine(Data_Generic.genericInt)((r.value0[2]).recValue(Prelude.unit))))(Data_Generic.fromSpine(Data_Generic.genericInt)((r.value0[3]).recValue(Prelude.unit)));
              };
              return Data_Maybe.Nothing.value;
          })(v.value1[0](Prelude.unit)));
      };
      return Data_Maybe.Nothing.value;
  }, function ($dollarq) {
      return new Data_Generic.SigProd("App.Model.Statistic.EventStatistic", [ {
          sigConstructor: "App.Model.Statistic.EventStatistic", 
          sigValues: [ function ($dollarq1) {
              return new Data_Generic.SigRecord([ {
                  recLabel: "computername", 
                  recValue: function ($dollarq2) {
                      return Data_Generic.toSignature(Data_Generic.genericString)(Data_Generic.anyProxy);
                  }
              }, {
                  recLabel: "eventId", 
                  recValue: function ($dollarq2) {
                      return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
                  }
              }, {
                  recLabel: "pictures", 
                  recValue: function ($dollarq2) {
                      return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
                  }
              }, {
                  recLabel: "prints", 
                  recValue: function ($dollarq2) {
                      return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
                  }
              } ]);
          } ]
      } ]);
  }, function (v) {
      return new Data_Generic.SProd("App.Model.Statistic.EventStatistic", [ function ($dollarq) {
          return new Data_Generic.SRecord([ {
              recLabel: "computername", 
              recValue: function ($dollarq1) {
                  return Data_Generic.toSpine(Data_Generic.genericString)(v.value0.computername);
              }
          }, {
              recLabel: "eventId", 
              recValue: function ($dollarq1) {
                  return Data_Generic.toSpine(Data_Generic.genericInt)(v.value0.eventId);
              }
          }, {
              recLabel: "pictures", 
              recValue: function ($dollarq1) {
                  return Data_Generic.toSpine(Data_Generic.genericInt)(v.value0.pictures);
              }
          }, {
              recLabel: "prints", 
              recValue: function ($dollarq1) {
                  return Data_Generic.toSpine(Data_Generic.genericInt)(v.value0.prints);
              }
          } ]);
      } ]);
  });                                                                                  
  var genericAllStatistics = new Data_Generic.Generic(function (v) {
      if (v instanceof Data_Generic.SProd && (v.value0 === "App.Model.Statistic.AllStatistics" && v.value1.length === 1)) {
          return Prelude.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(AllStatistics.create))((function (r) {
              if (r instanceof Data_Generic.SRecord && r.value0.length === 2) {
                  return Prelude.apply(Data_Maybe.applyMaybe)(Prelude.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(function (eventStatistics1) {
                      return function (monthlyStatistics1) {
                          return {
                              eventStatistics: eventStatistics1, 
                              monthlyStatistics: monthlyStatistics1
                          };
                      };
                  }))(Data_Generic.fromSpine(Data_Generic.genericArray(genericEventStatistic))((r.value0[0]).recValue(Prelude.unit))))(Data_Generic.fromSpine(Data_Generic.genericArray(genericMonthlyStatistic))((r.value0[1]).recValue(Prelude.unit)));
              };
              return Data_Maybe.Nothing.value;
          })(v.value1[0](Prelude.unit)));
      };
      return Data_Maybe.Nothing.value;
  }, function ($dollarq) {
      return new Data_Generic.SigProd("App.Model.Statistic.AllStatistics", [ {
          sigConstructor: "App.Model.Statistic.AllStatistics", 
          sigValues: [ function ($dollarq1) {
              return new Data_Generic.SigRecord([ {
                  recLabel: "eventStatistics", 
                  recValue: function ($dollarq2) {
                      return Data_Generic.toSignature(Data_Generic.genericArray(genericEventStatistic))(Data_Generic.anyProxy);
                  }
              }, {
                  recLabel: "monthlyStatistics", 
                  recValue: function ($dollarq2) {
                      return Data_Generic.toSignature(Data_Generic.genericArray(genericMonthlyStatistic))(Data_Generic.anyProxy);
                  }
              } ]);
          } ]
      } ]);
  }, function (v) {
      return new Data_Generic.SProd("App.Model.Statistic.AllStatistics", [ function ($dollarq) {
          return new Data_Generic.SRecord([ {
              recLabel: "eventStatistics", 
              recValue: function ($dollarq1) {
                  return Data_Generic.toSpine(Data_Generic.genericArray(genericEventStatistic))(v.value0.eventStatistics);
              }
          }, {
              recLabel: "monthlyStatistics", 
              recValue: function ($dollarq1) {
                  return Data_Generic.toSpine(Data_Generic.genericArray(genericMonthlyStatistic))(v.value0.monthlyStatistics);
              }
          } ]);
      } ]);
  });
  exports["AllStatistics"] = AllStatistics;
  exports["MonthlyStatistic"] = MonthlyStatistic;
  exports["EventStatistic"] = EventStatistic;
  exports["getMonthText"] = getMonthText;
  exports["genericEventStatistic"] = genericEventStatistic;
  exports["genericMonthlyStatistic"] = genericMonthlyStatistic;
  exports["genericAllStatistics"] = genericAllStatistics;
})(PS["App.Model.Statistic"] = PS["App.Model.Statistic"] || {});
(function(exports) {
    "use strict";

  //module App.Model.Date
  /*eslint-env node*/

  exports.toISOString = function iso8601(d){
    return d.toISOString();
  };

  exports.toLocalDatetime = function toLocalDatetime(d){
    return d.toISOString().substring(0, 11) + d.toString().substring(16, 21);
  };
  exports.fromLocalDatetimeImpl = function fromLocalDatetimeImpl(nothing){
    return function(just){
      return function(str){//YYYY-mm-DDTHH:MM
        var newD = new Date(); 
        var offset = newD.toString().substring(28, 33); //+0100
        var d = new Date(str + offset);
        if (d && d.toString() !== "Invalid Date"){
          return just(d);
        } else {
          return nothing;
        }
      };
    };
  };
})(PS["App.Model.Date"] = PS["App.Model.Date"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["App.Model.Date"];
  var Data_Date = PS["Data.Date"];
  var Data_Maybe = PS["Data.Maybe"];        
  var fromLocalDatetime = $foreign.fromLocalDatetimeImpl(Data_Maybe.Nothing.value)(Data_Maybe.Just.create);
  exports["fromLocalDatetime"] = fromLocalDatetime;
  exports["toLocalDatetime"] = $foreign.toLocalDatetime;
})(PS["App.Model.Date"] = PS["App.Model.Date"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var SQL = PS["SQL"];
  var Data_Foreign_Class = PS["Data.Foreign.Class"];
  var Data_Foreign = PS["Data.Foreign"];
  var Data_Generic = PS["Data.Generic"];
  var Data_Tuple = PS["Data.Tuple"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Either = PS["Data.Either"];
  var Data_Date = PS["Data.Date"];
  var App_Model_StrMap = PS["App.Model.StrMap"];
  var Data_Foreign_Index = PS["Data.Foreign.Index"];        
  var Session = (function () {
      function Session(value0) {
          this.value0 = value0;
      };
      Session.create = function (value0) {
          return new Session(value0);
      };
      return Session;
  })();
  var genericSession = new Data_Generic.Generic(function (v) {
      if (v instanceof Data_Generic.SProd && (v.value0 === "App.Model.Session.Session" && v.value1.length === 1)) {
          return Prelude.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Session.create))((function (r) {
              if (r instanceof Data_Generic.SRecord && r.value0.length === 3) {
                  return Prelude.apply(Data_Maybe.applyMaybe)(Prelude.apply(Data_Maybe.applyMaybe)(Prelude.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(function (createdOn1) {
                      return function (id1) {
                          return function (userId1) {
                              return {
                                  createdOn: createdOn1, 
                                  id: id1, 
                                  userId: userId1
                              };
                          };
                      };
                  }))(Data_Generic.fromSpine(Data_Date.genericDate)((r.value0[0]).recValue(Prelude.unit))))(Data_Generic.fromSpine(Data_Generic.genericInt)((r.value0[1]).recValue(Prelude.unit))))(Data_Generic.fromSpine(Data_Generic.genericInt)((r.value0[2]).recValue(Prelude.unit)));
              };
              return Data_Maybe.Nothing.value;
          })(v.value1[0](Prelude.unit)));
      };
      return Data_Maybe.Nothing.value;
  }, function ($dollarq) {
      return new Data_Generic.SigProd("App.Model.Session.Session", [ {
          sigConstructor: "App.Model.Session.Session", 
          sigValues: [ function ($dollarq1) {
              return new Data_Generic.SigRecord([ {
                  recLabel: "createdOn", 
                  recValue: function ($dollarq2) {
                      return Data_Generic.toSignature(Data_Date.genericDate)(Data_Generic.anyProxy);
                  }
              }, {
                  recLabel: "id", 
                  recValue: function ($dollarq2) {
                      return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
                  }
              }, {
                  recLabel: "userId", 
                  recValue: function ($dollarq2) {
                      return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
                  }
              } ]);
          } ]
      } ]);
  }, function (v) {
      return new Data_Generic.SProd("App.Model.Session.Session", [ function ($dollarq) {
          return new Data_Generic.SRecord([ {
              recLabel: "createdOn", 
              recValue: function ($dollarq1) {
                  return Data_Generic.toSpine(Data_Date.genericDate)(v.value0.createdOn);
              }
          }, {
              recLabel: "id", 
              recValue: function ($dollarq1) {
                  return Data_Generic.toSpine(Data_Generic.genericInt)(v.value0.id);
              }
          }, {
              recLabel: "userId", 
              recValue: function ($dollarq1) {
                  return Data_Generic.toSpine(Data_Generic.genericInt)(v.value0.userId);
              }
          } ]);
      } ]);
  });
  exports["Session"] = Session;
  exports["genericSession"] = genericSession;
})(PS["App.Model.Session"] = PS["App.Model.Session"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  exports._makeAff = function (cb) {
    return function(success, error) {
      return cb(function(e) {
        return function() {
          error(e);
        };
      })(function(v) {
        return function() {
          try {
            success(v);
          } catch (err) {
            error(err);
          }
        };
      })();
    }
  }

  exports._pure = function (nonCanceler, v) {
    return function(success, error) {
      try {
        success(v);
      } catch (err) {
        error(err);
      }

      return nonCanceler;
    };
  }

  exports._throwError = function (nonCanceler, e) {
    return function(success, error) {
      error(e);

      return nonCanceler;
    };
  }

  exports._fmap = function (f, aff) {
    return function(success, error) {
      return aff(function(v) {
        try {
          success(f(v));
        } catch (err) {
          error(err);
        }
      }, error);
    };
  }

  exports._bind = function (alwaysCanceler, aff, f) {
    return function(success, error) {
      var canceler1, canceler2;

      var isCanceled    = false;
      var requestCancel = false;

      var onCanceler = function(){};

      canceler1 = aff(function(v) {
        if (requestCancel) {
          isCanceled = true;

          return alwaysCanceler;
        } else {
          canceler2 = f(v)(success, error);

          onCanceler(canceler2);

          return canceler2;
        }
      }, error);

      return function(e) {
        return function(s, f) {
          requestCancel = true;

          if (canceler2 !== undefined) {
            return canceler2(e)(s, f);
          } else {
            return canceler1(e)(function(bool) {
              if (bool || isCanceled) {
                try {
                  s(true);
                } catch (err) {
                  f(err);
                }
              } else {
                onCanceler = function(canceler) {
                  canceler(e)(s, f);
                };
              }
            }, f);
          }
        };
      };
    };
  }

  exports._attempt = function (Left, Right, aff) {
    return function(success, error) {
      return aff(function(v) {
        try {
          success(Right(v));
        } catch (err) {
          error(err);
        }
      }, function(e) {
        try {
          success(Left(e));
        } catch (err) {
          error(err);
        }
      });
    };
  }

  exports._runAff = function (errorT, successT, aff) {
    return function() {
      return aff(function(v) {
        try {
          successT(v)();
        } catch (err) {
          errorT(err)();
        }
      }, function(e) {
        errorT(e)();
      });
    };
  }
})(PS["Control.Monad.Aff"] = PS["Control.Monad.Aff"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  exports.error = function (msg) {
    return new Error(msg);
  };

  exports.message = function (e) {
    return e.message;
  };

  exports.throwException = function (e) {
    return function () {
      throw e;
    };
  };
})(PS["Control.Monad.Eff.Exception"] = PS["Control.Monad.Eff.Exception"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["Control.Monad.Eff.Exception"];
  var Prelude = PS["Prelude"];
  var Data_Maybe = PS["Data.Maybe"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  exports["throwException"] = $foreign.throwException;
  exports["message"] = $foreign.message;
  exports["error"] = $foreign.error;
})(PS["Control.Monad.Eff.Exception"] = PS["Control.Monad.Eff.Exception"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Either = PS["Data.Either"];        
  var MonadError = function (__superclass_Prelude$dotMonad_0, catchError, throwError) {
      this["__superclass_Prelude.Monad_0"] = __superclass_Prelude$dotMonad_0;
      this.catchError = catchError;
      this.throwError = throwError;
  };
  var throwError = function (dict) {
      return dict.throwError;
  };                          
  var catchError = function (dict) {
      return dict.catchError;
  };
  exports["MonadError"] = MonadError;
  exports["catchError"] = catchError;
  exports["throwError"] = throwError;
})(PS["Control.Monad.Error.Class"] = PS["Control.Monad.Error.Class"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["Control.Monad.Aff"];
  var Prelude = PS["Prelude"];
  var Control_Alt = PS["Control.Alt"];
  var Control_Alternative = PS["Control.Alternative"];
  var Control_Monad_Cont_Class = PS["Control.Monad.Cont.Class"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Control_Monad_Eff_Class = PS["Control.Monad.Eff.Class"];
  var Control_Monad_Eff_Exception = PS["Control.Monad.Eff.Exception"];
  var Control_Monad_Error_Class = PS["Control.Monad.Error.Class"];
  var Control_Monad_Rec_Class = PS["Control.Monad.Rec.Class"];
  var Control_MonadPlus = PS["Control.MonadPlus"];
  var Control_Plus = PS["Control.Plus"];
  var Data_Either = PS["Data.Either"];
  var Data_Foldable = PS["Data.Foldable"];
  var Data_Function = PS["Data.Function"];
  var Data_Monoid = PS["Data.Monoid"];
  var runAff = function (ex) {
      return function (f) {
          return function (aff) {
              return $foreign._runAff(ex, f, aff);
          };
      };
  };
  var makeAff$prime = function (h) {
      return $foreign._makeAff(h);
  };
  var functorAff = new Prelude.Functor(function (f) {
      return function (fa) {
          return $foreign._fmap(f, fa);
      };
  });
  var attempt = function (aff) {
      return $foreign._attempt(Data_Either.Left.create, Data_Either.Right.create, aff);
  };
  var applyAff = new Prelude.Apply(function () {
      return functorAff;
  }, function (ff) {
      return function (fa) {
          return $foreign._bind(alwaysCanceler, ff, function (f) {
              return Prelude["<$>"](functorAff)(f)(fa);
          });
      };
  });
  var applicativeAff = new Prelude.Applicative(function () {
      return applyAff;
  }, function (v) {
      return $foreign._pure(nonCanceler, v);
  });
  var nonCanceler = Prelude["const"](Prelude.pure(applicativeAff)(false));
  var alwaysCanceler = Prelude["const"](Prelude.pure(applicativeAff)(true));
  var makeAff = function (h) {
      return makeAff$prime(function (e) {
          return function (a) {
              return Prelude["<$>"](Control_Monad_Eff.functorEff)(Prelude["const"](nonCanceler))(h(e)(a));
          };
      });
  };                                                       
  var bindAff = new Prelude.Bind(function () {
      return applyAff;
  }, function (fa) {
      return function (f) {
          return $foreign._bind(alwaysCanceler, fa, f);
      };
  });
  var monadAff = new Prelude.Monad(function () {
      return applicativeAff;
  }, function () {
      return bindAff;
  });
  var monadErrorAff = new Control_Monad_Error_Class.MonadError(function () {
      return monadAff;
  }, function (aff) {
      return function (ex) {
          return Prelude[">>="](bindAff)(attempt(aff))(Data_Either.either(ex)(Prelude.pure(applicativeAff)));
      };
  }, function (e) {
      return $foreign._throwError(nonCanceler, e);
  });
  exports["runAff"] = runAff;
  exports["nonCanceler"] = nonCanceler;
  exports["makeAff'"] = makeAff$prime;
  exports["makeAff"] = makeAff;
  exports["attempt"] = attempt;
  exports["functorAff"] = functorAff;
  exports["applyAff"] = applyAff;
  exports["applicativeAff"] = applicativeAff;
  exports["bindAff"] = bindAff;
  exports["monadAff"] = monadAff;
  exports["monadErrorAff"] = monadErrorAff;
})(PS["Control.Monad.Aff"] = PS["Control.Monad.Aff"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  // module Data.Nullable

  exports["null"] = null;

  exports.nullable = function(a, r, f) {
      return a == null ? r : f(a);
  };

  exports.notNull = function(x) {
      return x;
  };
})(PS["Data.Nullable"] = PS["Data.Nullable"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["Data.Nullable"];
  var Prelude = PS["Prelude"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Function = PS["Data.Function"];        
  var toNullable = Data_Maybe.maybe($foreign["null"])($foreign.notNull);
  var toMaybe = function (n) {
      return $foreign.nullable(n, Data_Maybe.Nothing.value, Data_Maybe.Just.create);
  };
  exports["toNullable"] = toNullable;
  exports["toMaybe"] = toMaybe;
})(PS["Data.Nullable"] = PS["Data.Nullable"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];        
  var $greater$eq$greater = function (dictBind) {
      return function (f) {
          return function (g) {
              return function (a) {
                  return Prelude[">>="](dictBind)(f(a))(g);
              };
          };
      };
  };
  var $eq$less$less = function (dictBind) {
      return function (f) {
          return function (m) {
              return Prelude[">>="](dictBind)(m)(f);
          };
      };
  };
  var $less$eq$less = function (dictBind) {
      return function (f) {
          return function (g) {
              return function (a) {
                  return $eq$less$less(dictBind)(f)(g(a));
              };
          };
      };
  };
  exports["<=<"] = $less$eq$less;
  exports[">=>"] = $greater$eq$greater;
})(PS["Control.Bind"] = PS["Control.Bind"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  // module Control.Monad.Eff.Unsafe

  exports.unsafeInterleaveEff = function (f) {
    return f;
  };
})(PS["Control.Monad.Eff.Unsafe"] = PS["Control.Monad.Eff.Unsafe"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["Control.Monad.Eff.Unsafe"];
  var Prelude = PS["Prelude"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];        
  var unsafePerformEff = function ($0) {
      return Control_Monad_Eff.runPure($foreign.unsafeInterleaveEff($0));
  };
  exports["unsafePerformEff"] = unsafePerformEff;
})(PS["Control.Monad.Eff.Unsafe"] = PS["Control.Monad.Eff.Unsafe"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Control_Monad_Eff_Unsafe = PS["Control.Monad.Eff.Unsafe"];
  var Control_Monad_Eff_Exception = PS["Control.Monad.Eff.Exception"];        
  var unsafeThrowException = function ($0) {
      return Control_Monad_Eff_Unsafe.unsafePerformEff(Control_Monad_Eff_Exception.throwException($0));
  };
  var unsafeThrow = function ($1) {
      return unsafeThrowException(Control_Monad_Eff_Exception.error($1));
  };
  exports["unsafeThrow"] = unsafeThrow;
  exports["unsafeThrowException"] = unsafeThrowException;
})(PS["Control.Monad.Eff.Exception.Unsafe"] = PS["Control.Monad.Eff.Exception.Unsafe"] || {});
(function(exports) {
  /* globals exports, JSON */
  "use strict";

  // module Global.Unsafe

  exports.unsafeStringify = function (x) {
    return JSON.stringify(x);
  };
})(PS["Global.Unsafe"] = PS["Global.Unsafe"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["Global.Unsafe"];
  exports["unsafeStringify"] = $foreign.unsafeStringify;
})(PS["Global.Unsafe"] = PS["Global.Unsafe"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Array = PS["Data.Array"];
  var Data_Tuple = PS["Data.Tuple"];
  var Data_Either = PS["Data.Either"];
  var Data_Foreign = PS["Data.Foreign"];
  var Data_Foreign_Index = PS["Data.Foreign.Index"];
  var Data_Function = PS["Data.Function"];
  var Data_Nullable = PS["Data.Nullable"];
  var Data_Generic = PS["Data.Generic"];
  var Data_Foldable = PS["Data.Foldable"];
  var Data_Traversable = PS["Data.Traversable"];
  var Data_List = PS["Data.List"];
  var Data_StrMap = PS["Data.StrMap"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Monad_Eff_Exception_Unsafe = PS["Control.Monad.Eff.Exception.Unsafe"];
  var Global_Unsafe = PS["Global.Unsafe"];
  var Type_Proxy = PS["Type.Proxy"];        
  var TaggedObject = (function () {
      function TaggedObject(value0) {
          this.value0 = value0;
      };
      TaggedObject.create = function (value0) {
          return new TaggedObject(value0);
      };
      return TaggedObject;
  })();
  var toForeignGeneric = function (dictGeneric) {
      return function (v) {
          var go = function (v1) {
              return function (v2) {
                  if (v2 instanceof Data_Generic.SNumber) {
                      return Data_Foreign.toForeign(v2.value0);
                  };
                  if (v2 instanceof Data_Generic.SInt) {
                      return Data_Foreign.toForeign(v2.value0);
                  };
                  if (v2 instanceof Data_Generic.SChar) {
                      return Data_Foreign.toForeign(v2.value0);
                  };
                  if (v2 instanceof Data_Generic.SString) {
                      return Data_Foreign.toForeign(v2.value0);
                  };
                  if (v2 instanceof Data_Generic.SBoolean) {
                      return Data_Foreign.toForeign(v2.value0);
                  };
                  if (v1 instanceof Data_Generic.SigArray && v2 instanceof Data_Generic.SArray) {
                      return Data_Foreign.toForeign(Prelude.map(Prelude.functorArray)(function ($167) {
                          return go(v1.value0(Prelude.unit))((function (v3) {
                              return v3(Prelude.unit);
                          })($167));
                      })(v2.value0));
                  };
                  if (v1 instanceof Data_Generic.SigRecord && v2 instanceof Data_Generic.SRecord) {
                      var pair = function (sig) {
                          return function (sp) {
                              if (sig.recLabel === sp.recLabel) {
                                  return new Data_Tuple.Tuple(sig.recLabel, go(sig.recValue(Prelude.unit))(sp.recValue(Prelude.unit)));
                              };
                              if (Prelude.otherwise) {
                                  return Control_Monad_Eff_Exception_Unsafe.unsafeThrow("Record fields do not match signature");
                              };
                              throw new Error("Failed pattern match at Data.Foreign.Generic line 135, column 5 - line 136, column 5: " + [ sig.constructor.name, sp.constructor.name ]);
                          };
                      };
                      var pairs = Data_Array.zipWith(pair)(Data_Array.sortBy(Data_Function.on(Prelude.compare(Prelude.ordString))(function (v3) {
                          return v3.recLabel;
                      }))(v1.value0))(Data_Array.sortBy(Data_Function.on(Prelude.compare(Prelude.ordString))(function (v3) {
                          return v3.recLabel;
                      }))(v2.value0));
                      return Data_Foreign.toForeign(Data_StrMap.fromList(Data_List.toList(Data_Foldable.foldableArray)(pairs)));
                  };
                  if (v1 instanceof Data_Generic.SigProd && (v1.value0 === "Data.Maybe.Maybe" && (v2 instanceof Data_Generic.SProd && (v2.value0 === "Data.Maybe.Nothing" && (v2.value1.length === 0 && v.maybeAsNull))))) {
                      return Data_Foreign.toForeign(Data_Nullable.toNullable(Data_Maybe.Nothing.value));
                  };
                  if (v1 instanceof Data_Generic.SigProd && (v1.value0 === "Data.Maybe.Maybe" && (v1.value1.length === 2 && ((v1.value1[0]).sigValues.length === 1 && (v2 instanceof Data_Generic.SProd && (v2.value0 === "Data.Maybe.Just" && (v2.value1.length === 1 && v.maybeAsNull))))))) {
                      return go((v1.value1[0]).sigValues[0](Prelude.unit))(v2.value1[0](Prelude.unit));
                  };
                  if (v1 instanceof Data_Generic.SigProd && (v1.value0 === "Data.Tuple.Tuple" && (v1.value1.length === 1 && ((v1.value1[0]).sigValues.length === 2 && (v2 instanceof Data_Generic.SProd && (v2.value0 === "Data.Tuple.Tuple" && (v2.value1.length === 2 && v.tupleAsArray))))))) {
                      return Data_Foreign.toForeign([ go((v1.value1[0]).sigValues[0](Prelude.unit))(v2.value1[0](Prelude.unit)), go((v1.value1[0]).sigValues[1](Prelude.unit))(v2.value1[1](Prelude.unit)) ]);
                  };
                  if (v1 instanceof Data_Generic.SigProd && (v1.value1.length === 1 && ((v1.value1[0]).sigValues.length === 1 && (v2 instanceof Data_Generic.SProd && (v2.value1.length === 1 && v.unwrapNewtypes))))) {
                      return go((v1.value1[0]).sigValues[0](Prelude.unit))(v2.value1[0](Prelude.unit));
                  };
                  if (v1 instanceof Data_Generic.SigProd && v2 instanceof Data_Generic.SProd) {
                      var $81 = Data_Foldable.find(Data_Foldable.foldableArray)(function (alt) {
                          return alt.sigConstructor === v2.value0;
                      })(v1.value1);
                      if ($81 instanceof Data_Maybe.Nothing) {
                          return Control_Monad_Eff_Exception_Unsafe.unsafeThrow("No signature for data constructor " + v2.value0);
                      };
                      if ($81 instanceof Data_Maybe.Just) {
                          var $82 = Data_Array.zipWith(function (sig) {
                              return function (sp) {
                                  return go(sig(Prelude.unit))(sp(Prelude.unit));
                              };
                          })($81.value0.sigValues)(v2.value1);
                          if ($82.length === 0) {
                              return Data_Foreign.toForeign(Data_StrMap.fromList(Data_List.singleton(new Data_Tuple.Tuple(v.sumEncoding.value0.tagFieldName, Data_Foreign.toForeign(v2.value0)))));
                          };
                          if ($82.length === 1 && v.unwrapSingleArgumentConstructors) {
                              return Data_Foreign.toForeign(Data_StrMap.fromList(Data_List.toList(Data_Foldable.foldableArray)([ new Data_Tuple.Tuple(v.sumEncoding.value0.tagFieldName, Data_Foreign.toForeign(v2.value0)), new Data_Tuple.Tuple(v.sumEncoding.value0.contentsFieldName, $82[0]) ])));
                          };
                          return Data_Foreign.toForeign(Data_StrMap.fromList(Data_List.toList(Data_Foldable.foldableArray)([ new Data_Tuple.Tuple(v.sumEncoding.value0.tagFieldName, Data_Foreign.toForeign(v2.value0)), new Data_Tuple.Tuple(v.sumEncoding.value0.contentsFieldName, Data_Foreign.toForeign($82)) ])));
                      };
                      throw new Error("Failed pattern match at Data.Foreign.Generic line 149, column 9 - line 161, column 3: " + [ $81.constructor.name ]);
                  };
                  return Control_Monad_Eff_Exception_Unsafe.unsafeThrow("Invalid spine for signature");
              };
          };
          return function ($168) {
              return go(Data_Generic.toSignature(dictGeneric)((Type_Proxy["Proxy"]).value))(Data_Generic.toSpine(dictGeneric)($168));
          };
      };
  };
  var toJSONGeneric = function (dictGeneric) {
      return function (opts) {
          return function ($169) {
              return Global_Unsafe.unsafeStringify(toForeignGeneric(dictGeneric)(opts)($169));
          };
      };
  };
  var readGeneric = function (dictGeneric) {
      return function (v) {
          var go = function (v1) {
              return function (f) {
                  if (v1 instanceof Data_Generic.SigNumber) {
                      return Prelude.map(Data_Either.functorEither)(Data_Generic.SNumber.create)(Data_Foreign.readNumber(f));
                  };
                  if (v1 instanceof Data_Generic.SigInt) {
                      return Prelude.map(Data_Either.functorEither)(Data_Generic.SInt.create)(Data_Foreign.readInt(f));
                  };
                  if (v1 instanceof Data_Generic.SigChar) {
                      return Prelude.map(Data_Either.functorEither)(Data_Generic.SChar.create)(Data_Foreign.readChar(f));
                  };
                  if (v1 instanceof Data_Generic.SigString) {
                      return Prelude.map(Data_Either.functorEither)(Data_Generic.SString.create)(Data_Foreign.readString(f));
                  };
                  if (v1 instanceof Data_Generic.SigBoolean) {
                      return Prelude.map(Data_Either.functorEither)(Data_Generic.SBoolean.create)(Data_Foreign.readBoolean(f));
                  };
                  if (v1 instanceof Data_Generic.SigArray) {
                      return Prelude.bind(Data_Either.bindEither)(Data_Foreign.readArray(f))(function (v2) {
                          return Prelude.bind(Data_Either.bindEither)(Data_Traversable["for"](Data_Either.applicativeEither)(Data_Traversable.traversableArray)(v2)(function (f2) {
                              return Prelude.bind(Data_Either.bindEither)(go(v1.value0(Prelude.unit))(f2))(function (v3) {
                                  return Prelude["return"](Data_Either.applicativeEither)(Prelude["const"](v3));
                              });
                          }))(function (v3) {
                              return Prelude["return"](Data_Either.applicativeEither)(new Data_Generic.SArray(v3));
                          });
                      });
                  };
                  if (v1 instanceof Data_Generic.SigRecord) {
                      return Prelude.bind(Data_Either.bindEither)(Data_Traversable["for"](Data_Either.applicativeEither)(Data_Traversable.traversableArray)(v1.value0)(function (prop) {
                          return Prelude.bind(Data_Either.bindEither)(Data_Foreign_Index["!"](Data_Foreign_Index.indexString)(f)(prop.recLabel))(function (v2) {
                              return Prelude.bind(Data_Either.bindEither)(go(prop.recValue(Prelude.unit))(v2))(function (v3) {
                                  return Prelude["return"](Data_Either.applicativeEither)({
                                      recLabel: prop.recLabel, 
                                      recValue: Prelude["const"](v3)
                                  });
                              });
                          });
                      }))(function (v2) {
                          return Prelude["return"](Data_Either.applicativeEither)(new Data_Generic.SRecord(v2));
                      });
                  };
                  if (v1 instanceof Data_Generic.SigProd && (v1.value1.length === 1 && ((v1.value1[0]).sigValues.length === 1 && v.unwrapNewtypes))) {
                      return Prelude.bind(Data_Either.bindEither)(go((v1.value1[0]).sigValues[0](Prelude.unit))(f))(function (v2) {
                          return Prelude["return"](Data_Either.applicativeEither)(new Data_Generic.SProd((v1.value1[0]).sigConstructor, [ function (v3) {
                              return v2;
                          } ]));
                      });
                  };
                  if (v1 instanceof Data_Generic.SigProd && (v1.value0 === "Data.Maybe.Maybe" && (v1.value1.length === 2 && ((v1.value1[0]).sigValues.length === 1 && v.maybeAsNull)))) {
                      var $117 = Data_Foreign.isNull(f) || Data_Foreign.isUndefined(f);
                      if ($117) {
                          return Prelude["return"](Data_Either.applicativeEither)(new Data_Generic.SProd("Data.Maybe.Nothing", [  ]));
                      };
                      if (!$117) {
                          return Prelude.bind(Data_Either.bindEither)(go((v1.value1[0]).sigValues[0](Prelude.unit))(f))(function (v2) {
                              return Prelude["return"](Data_Either.applicativeEither)(new Data_Generic.SProd("Data.Maybe.Just", [ function (v3) {
                                  return v2;
                              } ]));
                          });
                      };
                      throw new Error("Failed pattern match at Data.Foreign.Generic line 89, column 5 - line 93, column 3: " + [ $117.constructor.name ]);
                  };
                  if (v1 instanceof Data_Generic.SigProd && (v1.value0 === "Data.Tuple.Tuple" && (v1.value1.length === 1 && ((v1.value1[0]).sigValues.length === 2 && v.tupleAsArray)))) {
                      return Prelude.bind(Data_Either.bindEither)(Data_Foreign.readArray(f))(function (v2) {
                          if (v2.length === 2) {
                              return Prelude.bind(Data_Either.bindEither)(go((v1.value1[0]).sigValues[0](Prelude.unit))(v2[0]))(function (v3) {
                                  return Prelude.bind(Data_Either.bindEither)(go((v1.value1[0]).sigValues[1](Prelude.unit))(v2[1]))(function (v4) {
                                      return Prelude["return"](Data_Either.applicativeEither)(new Data_Generic.SProd("Data.Tuple.Tuple", [ function (v5) {
                                          return v3;
                                      }, function (v5) {
                                          return v4;
                                      } ]));
                                  });
                              });
                          };
                          return new Data_Either.Left(new Data_Foreign.TypeMismatch("array of length 2", "array"));
                      });
                  };
                  if (v1 instanceof Data_Generic.SigProd) {
                      return Prelude.bind(Data_Either.bindEither)(Prelude[">>="](Data_Either.bindEither)(Data_Foreign_Index.prop(v.sumEncoding.value0.tagFieldName)(f))(Data_Foreign.readString))(function (v2) {
                          var $142 = Data_Foldable.find(Data_Foldable.foldableArray)(function (alt) {
                              return alt.sigConstructor === v2;
                          })(v1.value1);
                          if ($142 instanceof Data_Maybe.Nothing) {
                              return new Data_Either.Left(new Data_Foreign.TypeMismatch("one of " + Prelude.show(Prelude.showArray(Prelude.showString))(Prelude.map(Prelude.functorArray)(function (v3) {
                                  return v3.sigConstructor;
                              })(v1.value1)), v2));
                          };
                          if ($142 instanceof Data_Maybe.Just && $142.value0.sigValues.length === 0) {
                              return Prelude["return"](Data_Either.applicativeEither)(new Data_Generic.SProd(v2, [  ]));
                          };
                          if ($142 instanceof Data_Maybe.Just && ($142.value0.sigValues.length === 1 && v.unwrapSingleArgumentConstructors)) {
                              return Prelude.bind(Data_Either.bindEither)(Data_Foreign_Index.prop(v.sumEncoding.value0.contentsFieldName)(f))(function (v3) {
                                  return Prelude.bind(Data_Either.bindEither)(go($142.value0.sigValues[0](Prelude.unit))(v3))(function (v4) {
                                      return Prelude["return"](Data_Either.applicativeEither)(new Data_Generic.SProd(v2, [ function (v5) {
                                          return v4;
                                      } ]));
                                  });
                              });
                          };
                          if ($142 instanceof Data_Maybe.Just) {
                              return Prelude.bind(Data_Either.bindEither)(Prelude[">>="](Data_Either.bindEither)(Data_Foreign_Index.prop(v.sumEncoding.value0.contentsFieldName)(f))(Data_Foreign.readArray))(function (v3) {
                                  return Prelude.bind(Data_Either.bindEither)(Data_Array.zipWithA(Data_Either.applicativeEither)(function (k) {
                                      return go(k(Prelude.unit));
                                  })($142.value0.sigValues)(v3))(function (v4) {
                                      return Prelude["return"](Data_Either.applicativeEither)(new Data_Generic.SProd(v2, Prelude.map(Prelude.functorArray)(Prelude["const"])(v4)));
                                  });
                              });
                          };
                          throw new Error("Failed pattern match at Data.Foreign.Generic line 105, column 9 - line 118, column 1: " + [ $142.constructor.name ]);
                      });
                  };
                  throw new Error("Failed pattern match at Data.Foreign.Generic line 68, column 3 - line 69, column 3: " + [ v1.constructor.name, f.constructor.name ]);
              };
          };
          var fromSpineUnsafe = function (sp) {
              var $160 = Data_Generic.fromSpine(dictGeneric)(sp);
              if ($160 instanceof Data_Maybe.Nothing) {
                  return Control_Monad_Eff_Exception_Unsafe.unsafeThrow("Invalid spine for signature");
              };
              if ($160 instanceof Data_Maybe.Just) {
                  return $160.value0;
              };
              throw new Error("Failed pattern match at Data.Foreign.Generic line 63, column 5 - line 67, column 3: " + [ $160.constructor.name ]);
          };
          return function ($170) {
              return Prelude.map(Data_Either.functorEither)(fromSpineUnsafe)(go(Data_Generic.toSignature(dictGeneric)((Type_Proxy["Proxy"]).value))($170));
          };
      };
  };
  var readJSONGeneric = function (dictGeneric) {
      return function (opts) {
          return Control_Bind[">=>"](Data_Either.bindEither)(Data_Foreign.parseJSON)(readGeneric(dictGeneric)(opts));
      };
  };
  var defaultOptions = {
      sumEncoding: new TaggedObject({
          tagFieldName: "tag", 
          contentsFieldName: "contents"
      }), 
      unwrapNewtypes: false, 
      unwrapSingleArgumentConstructors: true, 
      maybeAsNull: true, 
      tupleAsArray: false
  };
  exports["TaggedObject"] = TaggedObject;
  exports["toJSONGeneric"] = toJSONGeneric;
  exports["readJSONGeneric"] = readJSONGeneric;
  exports["toForeignGeneric"] = toForeignGeneric;
  exports["readGeneric"] = readGeneric;
  exports["defaultOptions"] = defaultOptions;
})(PS["Data.Foreign.Generic"] = PS["Data.Foreign.Generic"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var mediaTypeToString = function (v) {
      return v;
  };
  exports["mediaTypeToString"] = mediaTypeToString;
})(PS["Data.MediaType"] = PS["Data.MediaType"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Data_MediaType = PS["Data.MediaType"];           
  var applicationJSON = "application/json";
  exports["applicationJSON"] = applicationJSON;
})(PS["Data.MediaType.Common"] = PS["Data.MediaType.Common"] || {});
(function(exports) {
    "use strict";
  exports.parseIntImpl = function parseIntImpl(just){
    return function(nothing){
      return function(a){
        // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/parseInt
        return (/^(\-|\+)?([0-9]+)$/.test(a)) ? just(Number(a)) : nothing;
      }
    }
  }
  exports.toString = function toString(a){
    return a.toString();
  }
  exports.tupleRegexImpl = function tupleRegexImpl(tuple){
    return function(just){
      return function(nothing){
        var regex = /^\((.*)\,(.*)\)$/
        return function(string){
          var results = string.match(regex);
          return results[1] && results[2] ? just(tuple(results[1])(results[2])) : nothing;
        }
      }
    }
  }
})(PS["Data.Serializable"] = PS["Data.Serializable"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["Data.Serializable"];
  var Prelude = PS["Prelude"];
  var App_Model_Date = PS["App.Model.Date"];
  var Control_Monad_Eff_Exception = PS["Control.Monad.Eff.Exception"];
  var Data_Date_1 = PS["Data.Date"];
  var Data_Date_1 = PS["Data.Date"];
  var Data_Either = PS["Data.Either"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_String = PS["Data.String"];
  var Data_Traversable = PS["Data.Traversable"];
  var Data_Tuple = PS["Data.Tuple"];        
  var Serializable = function (deserialize, serialize) {
      this.deserialize = deserialize;
      this.serialize = serialize;
  };
  var serialize = function (dict) {
      return dict.serialize;
  };
  var serializableUnit = new Serializable(function (s) {
      var $7 = s === "unit";
      if ($7) {
          return new Data_Either.Right(Prelude.unit);
      };
      if (!$7) {
          return Data_Either.Left.create(Control_Monad_Eff_Exception.error("Unable to deserialize " + (s + " to Unit")));
      };
      throw new Error("Failed pattern match at Data.Serializable line 74, column 19 - line 77, column 1: " + [ $7.constructor.name ]);
  }, function (s) {
      return "unit";
  });
  var serializableString = new Serializable(function ($16) {
      return Data_Either.Right.create(Prelude.id(Prelude.categoryFn)($16));
  }, Prelude.id(Prelude.categoryFn));
  var $$parseInt = $foreign.parseIntImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
  var serializableInt = new Serializable(function (a) {
      return Data_Maybe.maybe(Data_Either.Left.create(Control_Monad_Eff_Exception.error("Unable to deserialize " + (a + " to Int"))))(Data_Either.Right.create)($$parseInt(a));
  }, $foreign.toString);
  var deserialize = function (dict) {
      return dict.deserialize;
  };
  var serializableTuple = function (dictSerializable) {
      return function (dictSerializable1) {
          return new Serializable(function (s) {
              return Prelude.bind(Data_Either.bindEither)(Data_Maybe.maybe(Data_Either.Left.create(Control_Monad_Eff_Exception.error("Unable to deserialize " + (s + " to Tuple"))))(Data_Either.Right.create)($foreign.tupleRegexImpl(Data_Tuple.Tuple.create)(Data_Maybe.Just.create)(Data_Maybe.Nothing.value)(s)))(function (v) {
                  return Prelude.bind(Data_Either.bindEither)(deserialize(dictSerializable)(v.value0))(function (v1) {
                      return Prelude.bind(Data_Either.bindEither)(deserialize(dictSerializable1)(v.value1))(function (v2) {
                          return Prelude["return"](Data_Either.applicativeEither)(new Data_Tuple.Tuple(v1, v2));
                      });
                  });
              });
          }, function (v) {
              return "(" + (serialize(dictSerializable)(v.value0) + ("," + (serialize(dictSerializable1)(v.value1) + ")")));
          });
      };
  };
  exports["Serializable"] = Serializable;
  exports["deserialize"] = deserialize;
  exports["serialize"] = serialize;
  exports["serializableInt"] = serializableInt;
  exports["serializableString"] = serializableString;
  exports["serializableTuple"] = serializableTuple;
  exports["serializableUnit"] = serializableUnit;
})(PS["Data.Serializable"] = PS["Data.Serializable"] || {});
(function(exports) {
  /* global exports */
  /* global XMLHttpRequest */
  /* global module */
  "use strict";

  // module Network.HTTP.Affjax

  // jshint maxparams: 5
  exports._ajax = function (mkHeader, options, canceler, errback, callback) {
    var platformSpecific = { };
    if (typeof module !== "undefined" && module.require) {
      // We are on node.js
      platformSpecific.newXHR = function () {
        var XHR = module.require("xhr2");
        return new XHR();
      };

      platformSpecific.fixupUrl = function (url) {
        var urllib = module.require("url");
        var u = urllib.parse(url);
        u.protocol = u.protocol || "http:";
        u.hostname = u.hostname || "localhost";
        return urllib.format(u);
      };

      platformSpecific.getResponse = function (xhr) {
        return xhr.response;
      };
    } else {
      // We are in the browser
      platformSpecific.newXHR = function () {
        return new XMLHttpRequest();
      };

      platformSpecific.fixupUrl = function (url) {
        return url || "/";
      };

      platformSpecific.getResponse = function (xhr) {
        return xhr.response;
      };
    }

    return function () {
      var xhr = platformSpecific.newXHR();
      var fixedUrl = platformSpecific.fixupUrl(options.url);
      xhr.open(options.method || "GET", fixedUrl, true, options.username, options.password);
      if (options.headers) {
        try {
          for (var i = 0, header; (header = options.headers[i]) != null; i++) {
            xhr.setRequestHeader(header.field, header.value);
          }
        }
        catch (e) {
          errback(e)();
        }
      }
      xhr.onerror = function () {
        errback(new Error("AJAX request failed: " + options.method + " " + options.url))();
      };
      xhr.onload = function () {
        callback({
          status: xhr.status,
          headers: xhr.getAllResponseHeaders().split("\n")
            .filter(function (header) {
              return header.length > 0;
            })
            .map(function (header) {
              var i = header.indexOf(":");
              return mkHeader(header.substring(0, i))(header.substring(i + 2));
            }),
          response: platformSpecific.getResponse(xhr)
        })();
      };
      xhr.responseType = options.responseType;
      xhr.withCredentials = options.withCredentials;
      xhr.send(options.content);
      return canceler(xhr);
    };
  };

  // jshint maxparams: 4
  exports._cancelAjax = function (xhr, cancelError, errback, callback) {
    return function () {
      try { xhr.abort(); } catch (e) { return callback(false)(); }
      return callback(true)();
    };
  };
})(PS["Network.HTTP.Affjax"] = PS["Network.HTTP.Affjax"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  // module Control.Monad.Eff.Ref

  exports.newRef = function (val) {
    return function () {
      return { value: val };
    };
  };

  exports.readRef = function (ref) {
    return function () {
      return ref.value;
    };
  };

  exports.writeRef = function (ref) {
    return function (val) {
      return function () {
        ref.value = val;
        return {};
      };
    };
  };
})(PS["Control.Monad.Eff.Ref"] = PS["Control.Monad.Eff.Ref"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["Control.Monad.Eff.Ref"];
  var Prelude = PS["Prelude"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  exports["writeRef"] = $foreign.writeRef;
  exports["readRef"] = $foreign.readRef;
  exports["newRef"] = $foreign.newRef;
})(PS["Control.Monad.Eff.Ref"] = PS["Control.Monad.Eff.Ref"] || {});
(function(exports) {
    "use strict";

  // module Unsafe.Coerce

  exports.unsafeCoerce = function(x) { return x; }
})(PS["Unsafe.Coerce"] = PS["Unsafe.Coerce"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["Unsafe.Coerce"];
  exports["unsafeCoerce"] = $foreign.unsafeCoerce;
})(PS["Unsafe.Coerce"] = PS["Unsafe.Coerce"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Data_Argonaut_Core = PS["Data.Argonaut.Core"];
  var Data_ArrayBuffer_Types = PS["Data.ArrayBuffer.Types"];
  var Data_FormURLEncoded_1 = PS["Data.FormURLEncoded"];
  var Data_FormURLEncoded_1 = PS["Data.FormURLEncoded"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_MediaType = PS["Data.MediaType"];
  var Data_MediaType_Common = PS["Data.MediaType.Common"];
  var Data_Tuple = PS["Data.Tuple"];
  var DOM_File_Types = PS["DOM.File.Types"];
  var DOM_Node_Types = PS["DOM.Node.Types"];
  var DOM_XHR_Types = PS["DOM.XHR.Types"];
  var Unsafe_Coerce = PS["Unsafe.Coerce"];        
  var Requestable = function (toRequest) {
      this.toRequest = toRequest;
  };
  var toRequest = function (dict) {
      return dict.toRequest;
  }; 
  var defaultToRequest = function ($0) {
      return Data_Tuple.Tuple.create(Data_Maybe.Nothing.value)(Unsafe_Coerce.unsafeCoerce($0));
  };
  var requestableBlob = new Requestable(defaultToRequest);          
  var requestableString = new Requestable(defaultToRequest);
  exports["Requestable"] = Requestable;
  exports["toRequest"] = toRequest;
  exports["requestableBlob"] = requestableBlob;
  exports["requestableString"] = requestableString;
})(PS["Network.HTTP.Affjax.Request"] = PS["Network.HTTP.Affjax.Request"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Data_Argonaut_Core = PS["Data.Argonaut.Core"];
  var Data_ArrayBuffer_Types = PS["Data.ArrayBuffer.Types"];
  var Data_Either = PS["Data.Either"];
  var Data_Foreign = PS["Data.Foreign"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_MediaType = PS["Data.MediaType"];
  var Data_MediaType_Common = PS["Data.MediaType.Common"];
  var Data_Tuple = PS["Data.Tuple"];
  var DOM_File_Types = PS["DOM.File.Types"];
  var DOM_Node_Types = PS["DOM.Node.Types"];
  var Unsafe_Coerce = PS["Unsafe.Coerce"];        
  var ArrayBufferResponse = (function () {
      function ArrayBufferResponse() {

      };
      ArrayBufferResponse.value = new ArrayBufferResponse();
      return ArrayBufferResponse;
  })();
  var BlobResponse = (function () {
      function BlobResponse() {

      };
      BlobResponse.value = new BlobResponse();
      return BlobResponse;
  })();
  var DocumentResponse = (function () {
      function DocumentResponse() {

      };
      DocumentResponse.value = new DocumentResponse();
      return DocumentResponse;
  })();
  var JSONResponse = (function () {
      function JSONResponse() {

      };
      JSONResponse.value = new JSONResponse();
      return JSONResponse;
  })();
  var StringResponse = (function () {
      function StringResponse() {

      };
      StringResponse.value = new StringResponse();
      return StringResponse;
  })();
  var Respondable = function (fromResponse, responseType) {
      this.fromResponse = fromResponse;
      this.responseType = responseType;
  }; 
  var responseTypeToString = function (v) {
      if (v instanceof ArrayBufferResponse) {
          return "arraybuffer";
      };
      if (v instanceof BlobResponse) {
          return "blob";
      };
      if (v instanceof DocumentResponse) {
          return "document";
      };
      if (v instanceof JSONResponse) {
          return "text";
      };
      if (v instanceof StringResponse) {
          return "text";
      };
      throw new Error("Failed pattern match at Network.HTTP.Affjax.Response line 50, column 1 - line 51, column 1: " + [ v.constructor.name ]);
  };
  var responseType = function (dict) {
      return dict.responseType;
  };                                                                                                                                                                 
  var responsableString = new Respondable(Data_Foreign.readString, new Data_Tuple.Tuple(Data_Maybe.Nothing.value, StringResponse.value));                               
  var fromResponse = function (dict) {
      return dict.fromResponse;
  };
  exports["ArrayBufferResponse"] = ArrayBufferResponse;
  exports["BlobResponse"] = BlobResponse;
  exports["DocumentResponse"] = DocumentResponse;
  exports["JSONResponse"] = JSONResponse;
  exports["StringResponse"] = StringResponse;
  exports["Respondable"] = Respondable;
  exports["fromResponse"] = fromResponse;
  exports["responseType"] = responseType;
  exports["responseTypeToString"] = responseTypeToString;
  exports["responsableString"] = responsableString;
})(PS["Network.HTTP.Affjax.Response"] = PS["Network.HTTP.Affjax.Response"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Data_MediaType = PS["Data.MediaType"];        
  var Accept = (function () {
      function Accept(value0) {
          this.value0 = value0;
      };
      Accept.create = function (value0) {
          return new Accept(value0);
      };
      return Accept;
  })();
  var ContentType = (function () {
      function ContentType(value0) {
          this.value0 = value0;
      };
      ContentType.create = function (value0) {
          return new ContentType(value0);
      };
      return ContentType;
  })();
  var RequestHeader = (function () {
      function RequestHeader(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      RequestHeader.create = function (value0) {
          return function (value1) {
              return new RequestHeader(value0, value1);
          };
      };
      return RequestHeader;
  })();
  var requestHeaderValue = function (v) {
      if (v instanceof Accept) {
          return Data_MediaType.mediaTypeToString(v.value0);
      };
      if (v instanceof ContentType) {
          return Data_MediaType.mediaTypeToString(v.value0);
      };
      if (v instanceof RequestHeader) {
          return v.value1;
      };
      throw new Error("Failed pattern match at Network.HTTP.RequestHeader line 29, column 1 - line 30, column 1: " + [ v.constructor.name ]);
  };
  var requestHeaderName = function (v) {
      if (v instanceof Accept) {
          return "Accept";
      };
      if (v instanceof ContentType) {
          return "Content-Type";
      };
      if (v instanceof RequestHeader) {
          return v.value0;
      };
      throw new Error("Failed pattern match at Network.HTTP.RequestHeader line 24, column 1 - line 25, column 1: " + [ v.constructor.name ]);
  };
  exports["Accept"] = Accept;
  exports["ContentType"] = ContentType;
  exports["RequestHeader"] = RequestHeader;
  exports["requestHeaderValue"] = requestHeaderValue;
  exports["requestHeaderName"] = requestHeaderName;
})(PS["Network.HTTP.RequestHeader"] = PS["Network.HTTP.RequestHeader"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];        
  var ResponseHeader = (function () {
      function ResponseHeader(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      ResponseHeader.create = function (value0) {
          return function (value1) {
              return new ResponseHeader(value0, value1);
          };
      };
      return ResponseHeader;
  })();
  var responseHeader = function (field) {
      return function (value) {
          return new ResponseHeader(field, value);
      };
  };
  exports["responseHeader"] = responseHeader;
})(PS["Network.HTTP.ResponseHeader"] = PS["Network.HTTP.ResponseHeader"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["Network.HTTP.Affjax"];
  var Prelude = PS["Prelude"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Monad_Aff = PS["Control.Monad.Aff"];
  var Control_Monad_Aff_AVar = PS["Control.Monad.Aff.AVar"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Control_Monad_Eff_Class = PS["Control.Monad.Eff.Class"];
  var Control_Monad_Eff_Exception = PS["Control.Monad.Eff.Exception"];
  var Control_Monad_Eff_Ref = PS["Control.Monad.Eff.Ref"];
  var Control_Monad_Error_Class = PS["Control.Monad.Error.Class"];
  var Data_Array = PS["Data.Array"];
  var Data_Either = PS["Data.Either"];
  var Data_Foldable = PS["Data.Foldable"];
  var Data_Foreign = PS["Data.Foreign"];
  var Data_Function = PS["Data.Function"];
  var Data_HTTP_Method_1 = PS["Data.HTTP.Method"];
  var Data_HTTP_Method_1 = PS["Data.HTTP.Method"];
  var Data_Int = PS["Data.Int"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_MediaType = PS["Data.MediaType"];
  var Data_Nullable = PS["Data.Nullable"];
  var Data_Tuple = PS["Data.Tuple"];
  var $$Math = PS["Math"];
  var DOM_XHR_Types = PS["DOM.XHR.Types"];
  var Network_HTTP_Affjax_Request = PS["Network.HTTP.Affjax.Request"];
  var Network_HTTP_Affjax_Response = PS["Network.HTTP.Affjax.Response"];
  var Network_HTTP_RequestHeader = PS["Network.HTTP.RequestHeader"];
  var Network_HTTP_ResponseHeader = PS["Network.HTTP.ResponseHeader"];
  var Network_HTTP_StatusCode = PS["Network.HTTP.StatusCode"];
  var cancelAjax = function (xhr) {
      return function (err) {
          return Control_Monad_Aff.makeAff(function (eb) {
              return function (cb) {
                  return $foreign._cancelAjax(xhr, err, eb, cb);
              };
          });
      };
  };
  var affjax$prime = function (dictRequestable) {
      return function (dictRespondable) {
          return function (req) {
              return function (eb) {
                  return function (cb) {
                      var responseSettings = Network_HTTP_Affjax_Response.responseType(dictRespondable);
                      var requestSettings = (function () {
                          var $49 = Prelude["<$>"](Data_Maybe.functorMaybe)(Network_HTTP_Affjax_Request.toRequest(dictRequestable))(req.content);
                          if ($49 instanceof Data_Maybe.Nothing) {
                              return new Data_Tuple.Tuple(Data_Maybe.Nothing.value, Data_Maybe.Nothing.value);
                          };
                          if ($49 instanceof Data_Maybe.Just) {
                              return new Data_Tuple.Tuple($49.value0.value0, new Data_Maybe.Just($49.value0.value1));
                          };
                          throw new Error("Failed pattern match at Network.HTTP.Affjax line 238, column 21 - line 242, column 3: " + [ $49.constructor.name ]);
                      })();
                      var fromResponse$prime = (function () {
                          var $53 = Data_Tuple.snd(responseSettings);
                          if ($53 instanceof Network_HTTP_Affjax_Response.JSONResponse) {
                              return Control_Bind["<=<"](Data_Either.bindEither)(Network_HTTP_Affjax_Response.fromResponse(dictRespondable))(Control_Bind["<=<"](Data_Either.bindEither)(Data_Foreign.parseJSON)(Data_Foreign.readString));
                          };
                          return Network_HTTP_Affjax_Response.fromResponse(dictRespondable);
                      })();
                      var cb$prime = function (res) {
                          var $56 = Prelude["<$>"](Data_Either.functorEither)(function (v) {
                              var $54 = {};
                              for (var $55 in res) {
                                  if (res.hasOwnProperty($55)) {
                                      $54[$55] = res[$55];
                                  };
                              };
                              $54.response = v;
                              return $54;
                          })(fromResponse$prime(res.response));
                          if ($56 instanceof Data_Either.Left) {
                              return eb(Control_Monad_Eff_Exception.error(Prelude.show(Data_Foreign.showForeignError)($56.value0)));
                          };
                          if ($56 instanceof Data_Either.Right) {
                              return cb($56.value0);
                          };
                          throw new Error("Failed pattern match at Network.HTTP.Affjax line 257, column 13 - line 261, column 3: " + [ $56.constructor.name ]);
                      };
                      var addHeader = function (h) {
                          return function (hs) {
                              if (h instanceof Data_Maybe.Just && !Data_Foldable.any(Data_Foldable.foldableArray)(Prelude.booleanAlgebraBoolean)(Data_Function.on(Prelude.eq(Prelude.eqString))(Network_HTTP_RequestHeader.requestHeaderName)(h.value0))(hs)) {
                                  return Data_Array.snoc(hs)(h.value0);
                              };
                              return hs;
                          };
                      };
                      var headers = addHeader(Prelude["<$>"](Data_Maybe.functorMaybe)(Network_HTTP_RequestHeader.ContentType.create)(Data_Tuple.fst(requestSettings)))(addHeader(Prelude["<$>"](Data_Maybe.functorMaybe)(Network_HTTP_RequestHeader.Accept.create)(Data_Tuple.fst(responseSettings)))(req.headers));
                      var req$prime = {
                          method: Data_HTTP_Method_1.print(req.method), 
                          url: req.url, 
                          headers: Prelude["<$>"](Prelude.functorArray)(function (h) {
                              return {
                                  field: Network_HTTP_RequestHeader.requestHeaderName(h), 
                                  value: Network_HTTP_RequestHeader.requestHeaderValue(h)
                              };
                          })(headers), 
                          content: Data_Nullable.toNullable(Data_Tuple.snd(requestSettings)), 
                          responseType: Network_HTTP_Affjax_Response.responseTypeToString(Data_Tuple.snd(responseSettings)), 
                          username: Data_Nullable.toNullable(req.username), 
                          password: Data_Nullable.toNullable(req.password), 
                          withCredentials: req.withCredentials
                      };
                      return $foreign._ajax(Network_HTTP_ResponseHeader.responseHeader, req$prime, cancelAjax, eb, cb$prime);
                  };
              };
          };
      };
  };
  var affjax = function (dictRequestable) {
      return function (dictRespondable) {
          return function ($74) {
              return Control_Monad_Aff["makeAff'"](affjax$prime(dictRequestable)(dictRespondable)($74));
          };
      };
  };
  exports["affjax"] = affjax;
})(PS["Network.HTTP.Affjax"] = PS["Network.HTTP.Affjax"] || {});
(function(exports) {
    "use strict";
  var Data_Either = PS["Data.Either"];
  var Control_Monad_Aff = PS["Control.Monad.Aff"];
  var Control_Monad_Eff_Exception = PS["Control.Monad.Eff.Exception"];
  var Control_Monad_Error_Class = PS["Control.Monad.Error.Class"];
  var DOM_File_Types = PS["DOM.File.Types"];
  var Data_Foreign_Generic = PS["Data.Foreign.Generic"];
  var Data_Generic = PS["Data.Generic"];
  var Data_HTTP_Method = PS["Data.HTTP.Method"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_MediaType_Common = PS["Data.MediaType.Common"];
  var Data_Serializable = PS["Data.Serializable"];
  var Debug_Trace = PS["Debug.Trace"];
  var Global = PS["Global"];
  var Network_HTTP_Affjax = PS["Network.HTTP.Affjax"];
  var Network_HTTP_RequestHeader = PS["Network.HTTP.RequestHeader"];
  var Prelude = PS["Prelude"];
  var Unsafe_Coerce = PS["Unsafe.Coerce"];
  var Network_HTTP_Affjax_Request = PS["Network.HTTP.Affjax.Request"];
  var Network_HTTP_Affjax_Response = PS["Network.HTTP.Affjax.Response"];        
  var FileUploadEndpoint = (function () {
      function FileUploadEndpoint(value0) {
          this.value0 = value0;
      };
      FileUploadEndpoint.create = function (value0) {
          return new FileUploadEndpoint(value0);
      };
      return FileUploadEndpoint;
  })();

  /**
 * ---------------------------------------
 */  
  var Endpoint = (function () {
      function Endpoint(value0) {
          this.value0 = value0;
      };
      Endpoint.create = function (value0) {
          return new Endpoint(value0);
      };
      return Endpoint;
  })();
  var parseOrThrow = function (dictGeneric) {
      return function (s) {
          return Data_Either.either(function (e) {
              return Control_Monad_Error_Class.throwError(Control_Monad_Aff.monadErrorAff)(Control_Monad_Eff_Exception.error("Failed to parse: " + s));
          })(Prelude["return"](Control_Monad_Aff.applicativeAff))(Data_Foreign_Generic.readJSONGeneric(dictGeneric)(Data_Foreign_Generic.defaultOptions)(s));
      };
  };

  /**
 *  Atm affjax does nat define File as Requestable. I could also make a PR for Affjax, but this is easier...
 */  
  var fileToBlob = Unsafe_Coerce.unsafeCoerce;
  var execFileUploadEndpoint = function (dictSerializable) {
      return function (dictGeneric) {
          return function (v) {
              return function (file) {
                  return function (a) {
                      var opts = {
                          method: new Data_Either.Left(Data_HTTP_Method.POST.value), 
                          url: Global["encodeURI"](v.value0.url + ("?params=" + Data_Serializable.serialize(dictSerializable)(a))), 
                          headers: [  ], 
                          content: Data_Maybe.Just.create(fileToBlob(file)), 
                          username: Data_Maybe.Nothing.value, 
                          password: Data_Maybe.Nothing.value, 
                          withCredentials: false
                      };
                      return Prelude[">>="](Control_Monad_Aff.bindAff)(Network_HTTP_Affjax.affjax(Network_HTTP_Affjax_Request.requestableBlob)(Network_HTTP_Affjax_Response.responsableString)(opts))(function ($29) {
                          return parseOrThrow(dictGeneric)((function (v1) {
                              return v1.response;
                          })($29));
                      });
                  };
              };
          };
      };
  };
  var execEndpoint_ = function (dictSerializable) {
      return function (dictGeneric) {
          return function (dictGeneric1) {
              return function (s) {
                  return function (v) {
                      return function (a) {
                          return function (b) {
                              if (v.value0.method instanceof Data_HTTP_Method.GET) {
                                  var url = Global["encodeURI"](s + (v.value0.url + ("?params=" + Data_Serializable.serialize(dictSerializable)(a))));
                                  var opts = {
                                      method: new Data_Either.Left(Data_HTTP_Method.GET.value), 
                                      url: url, 
                                      headers: [ new Network_HTTP_RequestHeader.ContentType(Data_MediaType_Common.applicationJSON) ], 
                                      content: Data_Maybe.Nothing.value, 
                                      username: Data_Maybe.Nothing.value, 
                                      password: Data_Maybe.Nothing.value, 
                                      withCredentials: false
                                  };
                                  return Prelude[">>="](Control_Monad_Aff.bindAff)(Network_HTTP_Affjax.affjax(Network_HTTP_Affjax_Request.requestableString)(Network_HTTP_Affjax_Response.responsableString)(opts))(function ($30) {
                                      return parseOrThrow(dictGeneric1)((function (v1) {
                                          return v1.response;
                                      })($30));
                                  });
                              };
                              var opts = {
                                  method: new Data_Either.Left(v.value0.method), 
                                  url: Global["encodeURI"](s + (v.value0.url + ("?params=" + Data_Serializable.serialize(dictSerializable)(a)))), 
                                  headers: [ new Network_HTTP_RequestHeader.ContentType(Data_MediaType_Common.applicationJSON) ], 
                                  content: Data_Maybe.Just.create(Data_Foreign_Generic.toJSONGeneric(dictGeneric)(Data_Foreign_Generic.defaultOptions)(b)), 
                                  username: Data_Maybe.Nothing.value, 
                                  password: Data_Maybe.Nothing.value, 
                                  withCredentials: false
                              };
                              return Prelude[">>="](Control_Monad_Aff.bindAff)(Network_HTTP_Affjax.affjax(Network_HTTP_Affjax_Request.requestableString)(Network_HTTP_Affjax_Response.responsableString)(opts))(function ($31) {
                                  return parseOrThrow(dictGeneric1)((function (v1) {
                                      return v1.response;
                                  })($31));
                              });
                          };
                      };
                  };
              };
          };
      };
  };

  /**
 *  relative path
 */  
  var execEndpoint = function (dictSerializable) {
      return function (dictGeneric) {
          return function (dictGeneric1) {
              return execEndpoint_(dictSerializable)(dictGeneric)(dictGeneric1)("");
          };
      };
  };
  exports["FileUploadEndpoint"] = FileUploadEndpoint;
  exports["Endpoint"] = Endpoint;
  exports["parseOrThrow"] = parseOrThrow;
  exports["fileToBlob"] = fileToBlob;
  exports["execFileUploadEndpoint"] = execFileUploadEndpoint;
  exports["execEndpoint"] = execEndpoint;
  exports["execEndpoint_"] = execEndpoint_;
})(PS["Endpoint.Client"] = PS["Endpoint.Client"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Data_HTTP_Method = PS["Data.HTTP.Method"];
  var Data_Tuple = PS["Data.Tuple"];
  var Data_Date = PS["Data.Date"];
  var Data_Maybe = PS["Data.Maybe"];
  var App_Model_Photobooth = PS["App.Model.Photobooth"];
  var App_Model_Event = PS["App.Model.Event"];
  var App_Model_SavedFile = PS["App.Model.SavedFile"];
  var App_Model_Statistic = PS["App.Model.Statistic"];
  var App_Model_Date = PS["App.Model.Date"];
  var App_Model_Session = PS["App.Model.Session"];
  var Endpoint_Client = PS["Endpoint.Client"];        
  var putPhotobooths = new Endpoint_Client.Endpoint({
      method: Data_HTTP_Method.PUT.value, 
      url: "/api/photobooths"
  });
  var putEvents = new Endpoint_Client.Endpoint({
      method: Data_HTTP_Method.PUT.value, 
      url: "/api/events"
  });
  var postPhotobooths = new Endpoint_Client.Endpoint({
      method: Data_HTTP_Method.POST.value, 
      url: "/api/photobooths"
  });
  var postEvents = new Endpoint_Client.Endpoint({
      method: Data_HTTP_Method.POST.value, 
      url: "/api/events"
  });
  var login = new Endpoint_Client.Endpoint({
      method: Data_HTTP_Method.POST.value, 
      url: "/api/login"
  });
  var getStatistics = new Endpoint_Client.Endpoint({
      method: Data_HTTP_Method.GET.value, 
      url: "/api/statistics/cname"
  });
  var getProfiles = new Endpoint_Client.Endpoint({
      method: Data_HTTP_Method.GET.value, 
      url: "/api/profiles/all"
  });
  var getPhotobooths = new Endpoint_Client.Endpoint({
      method: Data_HTTP_Method.GET.value, 
      url: "/api/photobooths/all"
  });
  var getEventsPaged = new Endpoint_Client.Endpoint({
      method: Data_HTTP_Method.GET.value, 
      url: "/api/events/paged"
  });
  var getEvents = new Endpoint_Client.Endpoint({
      method: Data_HTTP_Method.GET.value, 
      url: "/api/events/cname"
  });
  var deletePhotobooth = new Endpoint_Client.Endpoint({
      method: Data_HTTP_Method.DELETE.value, 
      url: "/api/photobooths"
  });
  var attachFile = new Endpoint_Client.FileUploadEndpoint({
      url: "/api/attachfiletoevent"
  });
  exports["login"] = login;
  exports["getProfiles"] = getProfiles;
  exports["getStatistics"] = getStatistics;
  exports["attachFile"] = attachFile;
  exports["getEventsPaged"] = getEventsPaged;
  exports["putEvents"] = putEvents;
  exports["postEvents"] = postEvents;
  exports["getEvents"] = getEvents;
  exports["deletePhotobooth"] = deletePhotobooth;
  exports["putPhotobooths"] = putPhotobooths;
  exports["postPhotobooths"] = postPhotobooths;
  exports["getPhotobooths"] = getPhotobooths;
})(PS["App.Endpoint"] = PS["App.Endpoint"] || {});
(function(exports) {
    "use strict";

  //module App.GUI.Router

  exports.setHash = function setHash(s){
    return function(){
      window.location.hash = encodeURI(s);
    };
  };

  exports.getHash = getHash;
  function getHash(){
    return decodeURI(window.location.hash);
  }

  exports.hashChanged = function hashChanged(handler) {
    return function() {
      window.addEventListener('hashchange', function() {
        var newHash = getHash();
        handler(newHash)();
      });
    };
  };
})(PS["App.GUI.Router"] = PS["App.GUI.Router"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Data_Either = PS["Data.Either"];
  var Data_Profunctor = PS["Data.Profunctor"];        
  var Choice = function (__superclass_Data$dotProfunctor$dotProfunctor_0, left, right) {
      this["__superclass_Data.Profunctor.Profunctor_0"] = __superclass_Data$dotProfunctor$dotProfunctor_0;
      this.left = left;
      this.right = right;
  };
  var right = function (dict) {
      return dict.right;
  };
  var left = function (dict) {
      return dict.left;
  };
  var choiceFn = new Choice(function () {
      return Data_Profunctor.profunctorFn;
  }, function (v) {
      return function (v1) {
          if (v1 instanceof Data_Either.Left) {
              return Data_Either.Left.create(v(v1.value0));
          };
          if (v1 instanceof Data_Either.Right) {
              return new Data_Either.Right(v1.value0);
          };
          throw new Error("Failed pattern match at Data.Profunctor.Choice line 18, column 3 - line 19, column 3: " + [ v.constructor.name, v1.constructor.name ]);
      };
  }, Prelude["<$>"](Data_Either.functorEither));
  exports["Choice"] = Choice;
  exports["right"] = right;
  exports["left"] = left;
  exports["choiceFn"] = choiceFn;
})(PS["Data.Profunctor.Choice"] = PS["Data.Profunctor.Choice"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Data_Tuple = PS["Data.Tuple"];        
  var MonadState = function (__superclass_Prelude$dotMonad_0, state) {
      this["__superclass_Prelude.Monad_0"] = __superclass_Prelude$dotMonad_0;
      this.state = state;
  };
  var state = function (dict) {
      return dict.state;
  };
  var modify = function (dictMonadState) {
      return function (f) {
          return state(dictMonadState)(function (s) {
              return new Data_Tuple.Tuple(Prelude.unit, f(s));
          });
      };
  };
  var gets = function (dictMonadState) {
      return function (f) {
          return state(dictMonadState)(function (s) {
              return new Data_Tuple.Tuple(f(s), s);
          });
      };
  };
  exports["MonadState"] = MonadState;
  exports["modify"] = modify;
  exports["gets"] = gets;
  exports["state"] = state;
})(PS["Control.Monad.State.Class"] = PS["Control.Monad.State.Class"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Data_Tuple = PS["Data.Tuple"];
  var Data_Either = PS["Data.Either"];
  var Control_Alt = PS["Control.Alt"];
  var Control_Alternative = PS["Control.Alternative"];
  var Control_Lazy = PS["Control.Lazy"];
  var Control_Monad_Cont_Class = PS["Control.Monad.Cont.Class"];
  var Control_Monad_Eff_Class = PS["Control.Monad.Eff.Class"];
  var Control_Monad_Error_Class = PS["Control.Monad.Error.Class"];
  var Control_Monad_Reader_Class = PS["Control.Monad.Reader.Class"];
  var Control_Monad_Rec_Class = PS["Control.Monad.Rec.Class"];
  var Control_Monad_State_Class = PS["Control.Monad.State.Class"];
  var Control_Monad_Trans = PS["Control.Monad.Trans"];
  var Control_Monad_Writer_Class = PS["Control.Monad.Writer.Class"];
  var Control_MonadPlus = PS["Control.MonadPlus"];
  var Control_Plus = PS["Control.Plus"];        
  var StateT = function (x) {
      return x;
  };
  var runStateT = function (v) {
      return v;
  };
  var monadStateT = function (dictMonad) {
      return new Prelude.Monad(function () {
          return applicativeStateT(dictMonad);
      }, function () {
          return bindStateT(dictMonad);
      });
  };
  var functorStateT = function (dictMonad) {
      return new Prelude.Functor(Prelude.liftM1(monadStateT(dictMonad)));
  };
  var bindStateT = function (dictMonad) {
      return new Prelude.Bind(function () {
          return applyStateT(dictMonad);
      }, function (v) {
          return function (f) {
              return function (s) {
                  return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(v(s))(function (v1) {
                      return runStateT(f(v1.value0))(v1.value1);
                  });
              };
          };
      });
  };
  var applyStateT = function (dictMonad) {
      return new Prelude.Apply(function () {
          return functorStateT(dictMonad);
      }, Prelude.ap(monadStateT(dictMonad)));
  };
  var applicativeStateT = function (dictMonad) {
      return new Prelude.Applicative(function () {
          return applyStateT(dictMonad);
      }, function (a) {
          return StateT(function (s) {
              return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Tuple.Tuple(a, s));
          });
      });
  };
  var monadStateStateT = function (dictMonad) {
      return new Control_Monad_State_Class.MonadState(function () {
          return monadStateT(dictMonad);
      }, function (f) {
          return StateT(function ($63) {
              return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(f($63));
          });
      });
  };
  exports["StateT"] = StateT;
  exports["runStateT"] = runStateT;
  exports["functorStateT"] = functorStateT;
  exports["applyStateT"] = applyStateT;
  exports["applicativeStateT"] = applicativeStateT;
  exports["bindStateT"] = bindStateT;
  exports["monadStateT"] = monadStateT;
  exports["monadStateStateT"] = monadStateStateT;
})(PS["Control.Monad.State.Trans"] = PS["Control.Monad.State.Trans"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Control_Comonad = PS["Control.Comonad"];
  var Control_Extend = PS["Control.Extend"];
  var Data_Foldable = PS["Data.Foldable"];
  var Data_Functor_Invariant = PS["Data.Functor.Invariant"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_Traversable = PS["Data.Traversable"];        
  var Identity = function (x) {
      return x;
  };
  var runIdentity = function (v) {
      return v;
  };
  var functorIdentity = new Prelude.Functor(function (f) {
      return function (v) {
          return f(v);
      };
  });
  var applyIdentity = new Prelude.Apply(function () {
      return functorIdentity;
  }, function (v) {
      return function (v1) {
          return v(v1);
      };
  });
  var bindIdentity = new Prelude.Bind(function () {
      return applyIdentity;
  }, function (v) {
      return function (f) {
          return f(v);
      };
  });
  var applicativeIdentity = new Prelude.Applicative(function () {
      return applyIdentity;
  }, Identity);
  var monadIdentity = new Prelude.Monad(function () {
      return applicativeIdentity;
  }, function () {
      return bindIdentity;
  });
  exports["Identity"] = Identity;
  exports["runIdentity"] = runIdentity;
  exports["functorIdentity"] = functorIdentity;
  exports["applyIdentity"] = applyIdentity;
  exports["applicativeIdentity"] = applicativeIdentity;
  exports["bindIdentity"] = bindIdentity;
  exports["monadIdentity"] = monadIdentity;
})(PS["Data.Identity"] = PS["Data.Identity"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Control_Monad_State_Class = PS["Control.Monad.State.Class"];
  var Control_Monad_State_Trans = PS["Control.Monad.State.Trans"];
  var Data_Identity = PS["Data.Identity"];
  var Data_Tuple = PS["Data.Tuple"];                   
  var runState = function (s) {
      return function ($0) {
          return Data_Identity.runIdentity(Control_Monad_State_Trans.runStateT(s)($0));
      };
  };
  var evalState = function (m) {
      return function (s) {
          return Data_Tuple.fst(runState(m)(s));
      };
  };
  exports["evalState"] = evalState;
  exports["runState"] = runState;
})(PS["Control.Monad.State"] = PS["Control.Monad.State"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Data_Tuple = PS["Data.Tuple"];
  var Data_Either = PS["Data.Either"];
  var Data_Profunctor = PS["Data.Profunctor"];
  var Data_Profunctor_Strong = PS["Data.Profunctor.Strong"];
  var Data_Profunctor_Choice = PS["Data.Profunctor.Choice"];        
  var Star = function (x) {
      return x;
  };
  var runStar = function (v) {
      return v;
  };
  var profunctorStar = function (dictFunctor) {
      return new Data_Profunctor.Profunctor(function (f) {
          return function (g) {
              return function (v) {
                  return function ($26) {
                      return Prelude.map(dictFunctor)(g)(v(f($26)));
                  };
              };
          };
      });
  };
  var strongStar = function (dictFunctor) {
      return new Data_Profunctor_Strong.Strong(function () {
          return profunctorStar(dictFunctor);
      }, function (v) {
          return function (v1) {
              return Prelude.map(dictFunctor)(function (v2) {
                  return new Data_Tuple.Tuple(v2, v1.value1);
              })(v(v1.value0));
          };
      }, function (v) {
          return function (v1) {
              return Prelude.map(dictFunctor)(Data_Tuple.Tuple.create(v1.value0))(v(v1.value1));
          };
      });
  };
  var choiceStar = function (dictApplicative) {
      return new Data_Profunctor_Choice.Choice(function () {
          return profunctorStar((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]());
      }, function (v) {
          return Star(Data_Either.either(function ($27) {
              return Prelude.map((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Either.Left.create)(v($27));
          })(function ($28) {
              return Prelude.pure(dictApplicative)(Data_Either.Right.create($28));
          }));
      }, function (v) {
          return Star(Data_Either.either(function ($29) {
              return Prelude.pure(dictApplicative)(Data_Either.Left.create($29));
          })(function ($30) {
              return Prelude.map((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Either.Right.create)(v($30));
          }));
      });
  };
  exports["Star"] = Star;
  exports["runStar"] = runStar;
  exports["profunctorStar"] = profunctorStar;
  exports["strongStar"] = strongStar;
  exports["choiceStar"] = choiceStar;
})(PS["Data.Profunctor.Star"] = PS["Data.Profunctor.Star"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Contravariant = function (cmap) {
      this.cmap = cmap;
  };
  var cmap = function (dict) {
      return dict.cmap;
  };
  exports["Contravariant"] = Contravariant;
  exports["cmap"] = cmap;
})(PS["Data.Functor.Contravariant"] = PS["Data.Functor.Contravariant"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Data_Profunctor_Strong = PS["Data.Profunctor.Strong"];
  var Data_Profunctor_Choice = PS["Data.Profunctor.Choice"];
  var Data_Profunctor_Star = PS["Data.Profunctor.Star"];
  var Data_Identity = PS["Data.Identity"];        
  var Wander = function (__superclass_Data$dotProfunctor$dotChoice$dotChoice_1, __superclass_Data$dotProfunctor$dotStrong$dotStrong_0, wander) {
      this["__superclass_Data.Profunctor.Choice.Choice_1"] = __superclass_Data$dotProfunctor$dotChoice$dotChoice_1;
      this["__superclass_Data.Profunctor.Strong.Strong_0"] = __superclass_Data$dotProfunctor$dotStrong$dotStrong_0;
      this.wander = wander;
  };
  var wanderStar = function (dictApplicative) {
      return new Wander(function () {
          return Data_Profunctor_Star.choiceStar(dictApplicative);
      }, function () {
          return Data_Profunctor_Star.strongStar((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]());
      }, function (t) {
          return function ($1) {
              return Data_Profunctor_Star.Star(t(dictApplicative)(Data_Profunctor_Star.runStar($1)));
          };
      });
  }; 
  var wander = function (dict) {
      return dict.wander;
  };
  exports["Wander"] = Wander;
  exports["wander"] = wander;
  exports["wanderStar"] = wanderStar;
})(PS["Data.Lens.Internal.Wander"] = PS["Data.Lens.Internal.Wander"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Data_Profunctor = PS["Data.Profunctor"];
  var Data_Profunctor_Choice = PS["Data.Profunctor.Choice"];
  var Data_Profunctor_Strong = PS["Data.Profunctor.Strong"];
  var Control_Monad_State = PS["Control.Monad.State"];
  var Control_Monad_State_Class = PS["Control.Monad.State.Class"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_Lens = PS["Data.Lens"];
  var Data_Profunctor_Star = PS["Data.Profunctor.Star"];
  var Data_Traversable = PS["Data.Traversable"];
  var Data_Tuple = PS["Data.Tuple"];
  var Data_Either = PS["Data.Either"];
  var Data_Functor_Contravariant = PS["Data.Functor.Contravariant"];
  var Data_Lens_Internal_Wander = PS["Data.Lens.Internal.Wander"];
  var Control_Monad_State_Trans = PS["Control.Monad.State.Trans"];
  var Data_Identity = PS["Data.Identity"];        
  var Handler = function (x) {
      return x;
  };
  var Sink = (function () {
      function Sink(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      Sink.create = function (value0) {
          return function (value1) {
              return new Sink(value0, value1);
          };
      };
      return Sink;
  })();
  var withView = function (f) {
      return function (v) {
          return function (s) {
              return function (h) {
                  return Prelude.map(Control_Monad_Eff.functorEff)(f)(v(s)(h));
              };
          };
      };
  };
  var $$with = function (f) {
      return function (s) {
          return function (h) {
              var $39 = f(s)(h);
              return $39(s)(h);
          };
      };
  };
  var uiSemigroup = function (dictSemigroup) {
      return new Prelude.Semigroup(function (v) {
          return function (v1) {
              return function (s) {
                  return function (h) {
                      return Prelude["<*>"](Control_Monad_Eff.applyEff)(Prelude["<$>"](Control_Monad_Eff.functorEff)(Prelude.append(dictSemigroup))(v(s)(h)))(v1(s)(h));
                  };
              };
          };
      });
  };
  var uiMonoid = function (dictMonoid) {
      return new Data_Monoid.Monoid(function () {
          return uiSemigroup(dictMonoid["__superclass_Prelude.Semigroup_0"]());
      }, function (v) {
          return function (v1) {
              return Prelude.pure(Control_Monad_Eff.applicativeEff)(Data_Monoid.mempty(dictMonoid));
          };
      });
  };
  var ui = function (v) {
      return function (v1) {
          return function (v2) {
              return Prelude.pure(Control_Monad_Eff.applicativeEff)(v);
          };
      };
  };
  var toSink = function (v) {
      return function (s) {
          return new Sink(s, v(s));
      };
  };
  var runUI = function (v) {
      return v;
  };
  var runSink = function (v) {
      return v.value1;
  };
  var runHandler = function (v) {
      return v;
  };
  var inline = function (go) {
      return function (v) {
          return function (v1) {
              return go;
          };
      };
  };
  var handlerContravariant = new Data_Functor_Contravariant.Contravariant(function (f) {
      return function (v) {
          return function ($83) {
              return v(f($83));
          };
      };
  });
  var sinkFunctor = new Prelude.Functor(function (f) {
      return function (v) {
          return new Sink(f(v.value0), function ($84) {
              return v.value1(Data_Functor_Contravariant.cmap(handlerContravariant)(f)($84));
          });
      };
  });
  var sinkApply = function (dictSemigroup) {
      return new Prelude.Apply(function () {
          return sinkFunctor;
      }, function (v) {
          return function (v1) {
              return new Sink(v.value0(v1.value0), function (h) {
                  return Prelude["<*>"](Control_Monad_Eff.applyEff)(Prelude["<$>"](Control_Monad_Eff.functorEff)(Prelude.append(dictSemigroup))(v.value1(Data_Functor_Contravariant.cmap(handlerContravariant)(function (v2) {
                      return v2(v1.value0);
                  })(h))))(v1.value1(Data_Functor_Contravariant.cmap(handlerContravariant)(function (v2) {
                      return v.value0(v2);
                  })(h)));
              });
          };
      });
  };
  var sinkApplicative = function (dictMonoid) {
      return new Prelude.Applicative(function () {
          return sinkApply(dictMonoid["__superclass_Prelude.Semigroup_0"]());
      }, function (x) {
          return new Sink(x, function (v) {
              return Prelude.pure(Control_Monad_Eff.applicativeEff)(Data_Monoid.mempty(dictMonoid));
          });
      });
  };
  var uiProfunctor = new Data_Profunctor.Profunctor(function (f) {
      return function (g) {
          return function (v) {
              return function (s) {
                  return function (h) {
                      return v(f(s))(Data_Functor_Contravariant.cmap(handlerContravariant)(g)(h));
                  };
              };
          };
      };
  });
  var uiChoice = function (dictMonoid) {
      return new Data_Profunctor_Choice.Choice(function () {
          return uiProfunctor;
      }, function (v) {
          return function (s) {
              return function (h) {
                  return Data_Either.either(Prelude.flip(v)(Data_Functor_Contravariant.cmap(handlerContravariant)(Data_Either.Left.create)(h)))(function (v1) {
                      return Prelude.pure(Control_Monad_Eff.applicativeEff)(Data_Monoid.mempty(dictMonoid));
                  })(s);
              };
          };
      }, function (v) {
          return function (s) {
              return function (h) {
                  return Data_Either.either(function (v1) {
                      return Prelude.pure(Control_Monad_Eff.applicativeEff)(Data_Monoid.mempty(dictMonoid));
                  })(Prelude.flip(v)(Data_Functor_Contravariant.cmap(handlerContravariant)(Data_Either.Right.create)(h)))(s);
              };
          };
      });
  };
  var uiStrong = new Data_Profunctor_Strong.Strong(function () {
      return uiProfunctor;
  }, function (v) {
      return function (v1) {
          return function (h) {
              return v(v1.value0)(Data_Functor_Contravariant.cmap(handlerContravariant)(function (r) {
                  return new Data_Tuple.Tuple(r, v1.value1);
              })(h));
          };
      };
  }, function (v) {
      return function (v1) {
          return function (h) {
              return v(v1.value1)(Data_Functor_Contravariant.cmap(handlerContravariant)(function (r) {
                  return new Data_Tuple.Tuple(v1.value0, r);
              })(h));
          };
      };
  });
  var fromSink = function (f) {
      return function ($85) {
          return runSink(f($85));
      };
  };
  var traversal = function (dictMonoid) {
      return function (t) {
          return function ($86) {
              return fromSink(Data_Profunctor_Star.runStar(t(Data_Lens_Internal_Wander.wanderStar(sinkApplicative(dictMonoid)))(Data_Profunctor_Star.Star(toSink($86)))));
          };
      };
  };
  var foreach = function (dictMonoid) {
      return function (dictTraversable) {
          return function (f) {
              var indices = function (g) {
                  return function (t) {
                      return Control_Monad_State.evalState(Data_Traversable.traverse(dictTraversable)(Control_Monad_State_Trans.applicativeStateT(Data_Identity.monadIdentity))(function (x) {
                          return Control_Monad_State_Class.state(Control_Monad_State_Trans.monadStateStateT(Data_Identity.monadIdentity))(function (i) {
                              return new Data_Tuple.Tuple(g(i)(x), i + 1 | 0);
                          });
                      })(t))(0);
                  };
              };
              return fromSink(function ($87) {
                  return Data_Traversable.sequence(dictTraversable)(sinkApplicative(dictMonoid))(indices(function ($88) {
                      return toSink(f($88));
                  })($87));
              });
          };
      };
  };
  exports["Handler"] = Handler;
  exports["inline"] = inline;
  exports["foreach"] = foreach;
  exports["traversal"] = traversal;
  exports["withView"] = withView;
  exports["with"] = $$with;
  exports["ui"] = ui;
  exports["runUI"] = runUI;
  exports["runHandler"] = runHandler;
  exports["uiProfunctor"] = uiProfunctor;
  exports["uiChoice"] = uiChoice;
  exports["uiStrong"] = uiStrong;
  exports["uiSemigroup"] = uiSemigroup;
  exports["uiMonoid"] = uiMonoid;
  exports["handlerContravariant"] = handlerContravariant;
})(PS["OpticUI.Core"] = PS["OpticUI.Core"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var OpticUI_Core = PS["OpticUI.Core"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Control_Monad_Eff_Ref = PS["Control.Monad.Eff.Ref"];
  var Control_Monad_Aff = PS["Control.Monad.Aff"];
  var Control_Monad_Eff_Exception = PS["Control.Monad.Eff.Exception"];
  var Data_Either = PS["Data.Either"];
  var Data_Monoid = PS["Data.Monoid"];        
  var Async = (function () {
      function Async(value0) {
          this.value0 = value0;
      };
      Async.create = function (value0) {
          return new Async(value0);
      };
      return Async;
  })();
  var onResult = function (dictMonoid) {
      return function (s) {
          return function (f) {
              return OpticUI_Core["with"](function (v) {
                  return function (v1) {
                      return OpticUI_Core.inline(function __do() {
                          Control_Monad_Eff_Ref.writeRef(v.value0)(Data_Either.either(f)(s))();
                          return Data_Monoid.mempty(dictMonoid);
                      });
                  };
              });
          };
      };
  };
  var async = function (go) {
      return function __do() {
          var v = Control_Monad_Eff_Ref.newRef(Prelude["const"](Prelude.pure(Control_Monad_Eff.applicativeEff)(Prelude.unit)))();
          var s = function (a) {
              return function __do() {
                  var v1 = Control_Monad_Eff_Ref.readRef(v)();
                  return v1(new Data_Either.Right(a))();
              };
          };
          var f = function (e) {
              return function __do() {
                  var v1 = Control_Monad_Eff_Ref.readRef(v)();
                  return v1(new Data_Either.Left(e))();
              };
          };
          Control_Monad_Aff.runAff(f)(s)(go)();
          return new Async(v);
      };
  };
  exports["Async"] = Async;
  exports["async"] = async;
  exports["onResult"] = onResult;
})(PS["OpticUI.Components.Async"] = PS["OpticUI.Components.Async"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Control_MonadPlus = PS["Control.MonadPlus"];
  var Data_Either = PS["Data.Either"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Profunctor = PS["Data.Profunctor"];
  var Data_Profunctor_Choice = PS["Data.Profunctor.Choice"];
  var Data_Lens_Types_1 = PS["Data.Lens.Types"];
  var Data_Lens_Types_1 = PS["Data.Lens.Types"];
  var Data_Lens_Internal_Market = PS["Data.Lens.Internal.Market"];
  var Data_Lens_Internal_Tagged = PS["Data.Lens.Internal.Tagged"];
  var prism = function (to) {
      return function (fro) {
          return function (dictChoice) {
              return function (pab) {
                  return Data_Profunctor.dimap(dictChoice["__superclass_Data.Profunctor.Profunctor_0"]())(fro)(Data_Either.either(Prelude.id(Prelude.categoryFn))(Prelude.id(Prelude.categoryFn)))(Data_Profunctor_Choice.right(dictChoice)(Data_Profunctor.rmap(dictChoice["__superclass_Data.Profunctor.Profunctor_0"]())(to)(pab)));
              };
          };
      };
  };
  var prism$prime = function (to) {
      return function (fro) {
          return function (dictChoice) {
              return prism(to)(function (s) {
                  return Data_Maybe.maybe(new Data_Either.Left(s))(Data_Either.Right.create)(fro(s));
              })(dictChoice);
          };
      };
  };
  exports["prism'"] = prism$prime;
  exports["prism"] = prism;
})(PS["Data.Lens.Prism"] = PS["Data.Lens.Prism"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var OpticUI_Components_Async = PS["OpticUI.Components.Async"];
  var Data_Lens = PS["Data.Lens"];
  var Data_Maybe = PS["Data.Maybe"];
  var Control_Monad_Eff_Exception = PS["Control.Monad.Eff.Exception"];
  var Data_Lens_Prism = PS["Data.Lens.Prism"];        
  var Initial = (function () {
      function Initial() {

      };
      Initial.value = new Initial();
      return Initial;
  })();
  var Busy = (function () {
      function Busy(value0) {
          this.value0 = value0;
      };
      Busy.create = function (value0) {
          return new Busy(value0);
      };
      return Busy;
  })();
  var Errored = (function () {
      function Errored(value0) {
          this.value0 = value0;
      };
      Errored.create = function (value0) {
          return new Errored(value0);
      };
      return Errored;
  })();
  var Done = (function () {
      function Done(value0) {
          this.value0 = value0;
      };
      Done.create = function (value0) {
          return new Done(value0);
      };
      return Done;
  })();
  var _Initial = function (dictChoice) {
      var l = function (v) {
          if (v instanceof Initial) {
              return new Data_Maybe.Just(Prelude.unit);
          };
          return Data_Maybe.Nothing.value;
      };
      return Data_Lens_Prism["prism'"](Prelude["const"](Initial.value))(l)(dictChoice);
  };
  var _Errored = function (dictChoice) {
      var l = function (v) {
          if (v instanceof Errored) {
              return new Data_Maybe.Just(v.value0);
          };
          return Data_Maybe.Nothing.value;
      };
      return Data_Lens_Prism["prism'"](Errored.create)(l)(dictChoice);
  };
  var _Done = function (dictChoice) {
      var l = function (v) {
          if (v instanceof Done) {
              return new Data_Maybe.Just(v.value0);
          };
          return Data_Maybe.Nothing.value;
      };
      return Data_Lens_Prism["prism'"](Done.create)(l)(dictChoice);
  };
  var _Busy = function (dictChoice) {
      var l = function (v) {
          if (v instanceof Busy) {
              return new Data_Maybe.Just(v.value0);
          };
          return Data_Maybe.Nothing.value;
      };
      return Data_Lens_Prism["prism'"](Busy.create)(l)(dictChoice);
  };
  exports["Initial"] = Initial;
  exports["Busy"] = Busy;
  exports["Errored"] = Errored;
  exports["Done"] = Done;
  exports["_Done"] = _Done;
  exports["_Errored"] = _Errored;
  exports["_Busy"] = _Busy;
  exports["_Initial"] = _Initial;
})(PS["App.Model.Async"] = PS["App.Model.Async"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var App_Model_Async = PS["App.Model.Async"];
  var App_Model_Event = PS["App.Model.Event"];
  var App_Model_Photobooth = PS["App.Model.Photobooth"];
  var App_Model_Profile = PS["App.Model.Profile"];
  var App_Model_SavedFile = PS["App.Model.SavedFile"];
  var App_Model_Session = PS["App.Model.Session"];
  var App_Model_Statistic = PS["App.Model.Statistic"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var DOM_File_Types = PS["DOM.File.Types"];
  var Data_Date = PS["Data.Date"];
  var Data_Generic = PS["Data.Generic"];
  var Data_Lens = PS["Data.Lens"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Profunctor_Strong = PS["Data.Profunctor.Strong"];
  var Prelude = PS["Prelude"];
  var Data_Lens_Lens = PS["Data.Lens.Lens"];        
  var LoginPage = (function () {
      function LoginPage() {

      };
      LoginPage.value = new LoginPage();
      return LoginPage;
  })();
  var PhotoboothsPage = (function () {
      function PhotoboothsPage() {

      };
      PhotoboothsPage.value = new PhotoboothsPage();
      return PhotoboothsPage;
  })();
  var EventsPage = (function () {
      function EventsPage(value0, value1, value2) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value2;
      };
      EventsPage.create = function (value0) {
          return function (value1) {
              return function (value2) {
                  return new EventsPage(value0, value1, value2);
              };
          };
      };
      return EventsPage;
  })();
  var StatisticsPage = (function () {
      function StatisticsPage(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      StatisticsPage.create = function (value0) {
          return function (value1) {
              return new StatisticsPage(value0, value1);
          };
      };
      return StatisticsPage;
  })();
  var initialState = function (initRoute) {
      return function __do() {
          var v = Data_Date.now();
          return {
              route: initRoute, 
              session: Data_Maybe.Nothing.value, 
              photobooths: App_Model_Async.Initial.value, 
              profiles: App_Model_Async.Initial.value, 
              loginPage: {
                  username: "", 
                  password: "", 
                  loggingIn: App_Model_Async.Initial.value
              }, 
              statisticsPage: {
                  events: App_Model_Async.Initial.value, 
                  statistics: App_Model_Async.Initial.value
              }, 
              eventsPage: {
                  "new": {
                      model: {
                          id: Data_Maybe.Nothing.value, 
                          computername: "", 
                          name: "", 
                          datefrom: v, 
                          dateuntil: v, 
                          profile: "", 
                          files: [  ]
                      }, 
                      state: App_Model_Async.Initial.value
                  }, 
                  editing: Data_Maybe.Nothing.value, 
                  deleting: Data_Maybe.Nothing.value, 
                  events: App_Model_Async.Initial.value
              }, 
              photoboothsPage: {
                  "new": {
                      model: {
                          id: Data_Maybe.Nothing.value, 
                          computername: "", 
                          alias: "", 
                          defaultprofile: ""
                      }, 
                      state: App_Model_Async.Initial.value
                  }, 
                  editing: Data_Maybe.Nothing.value, 
                  deleting: Data_Maybe.Nothing.value
              }
          };
      };
  };
  var genericRoute = new Data_Generic.Generic(function (v) {
      if (v instanceof Data_Generic.SProd && (v.value0 === "App.GUI.State.LoginPage" && v.value1.length === 0)) {
          return new Data_Maybe.Just(LoginPage.value);
      };
      if (v instanceof Data_Generic.SProd && (v.value0 === "App.GUI.State.PhotoboothsPage" && v.value1.length === 0)) {
          return new Data_Maybe.Just(PhotoboothsPage.value);
      };
      if (v instanceof Data_Generic.SProd && (v.value0 === "App.GUI.State.EventsPage" && v.value1.length === 3)) {
          return Prelude.apply(Data_Maybe.applyMaybe)(Prelude.apply(Data_Maybe.applyMaybe)(Prelude.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(EventsPage.create))(Data_Generic.fromSpine(Data_Generic.genericString)(v.value1[0](Prelude.unit))))(Data_Generic.fromSpine(Data_Generic.genericString)(v.value1[1](Prelude.unit))))(Data_Generic.fromSpine(Data_Generic.genericInt)(v.value1[2](Prelude.unit)));
      };
      if (v instanceof Data_Generic.SProd && (v.value0 === "App.GUI.State.StatisticsPage" && v.value1.length === 2)) {
          return Prelude.apply(Data_Maybe.applyMaybe)(Prelude.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(StatisticsPage.create))(Data_Generic.fromSpine(Data_Generic.genericString)(v.value1[0](Prelude.unit))))(Data_Generic.fromSpine(Data_Generic.genericString)(v.value1[1](Prelude.unit)));
      };
      return Data_Maybe.Nothing.value;
  }, function ($dollarq) {
      return new Data_Generic.SigProd("App.GUI.State.Route", [ {
          sigConstructor: "App.GUI.State.LoginPage", 
          sigValues: [  ]
      }, {
          sigConstructor: "App.GUI.State.PhotoboothsPage", 
          sigValues: [  ]
      }, {
          sigConstructor: "App.GUI.State.EventsPage", 
          sigValues: [ function ($dollarq1) {
              return Data_Generic.toSignature(Data_Generic.genericString)(Data_Generic.anyProxy);
          }, function ($dollarq1) {
              return Data_Generic.toSignature(Data_Generic.genericString)(Data_Generic.anyProxy);
          }, function ($dollarq1) {
              return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
          } ]
      }, {
          sigConstructor: "App.GUI.State.StatisticsPage", 
          sigValues: [ function ($dollarq1) {
              return Data_Generic.toSignature(Data_Generic.genericString)(Data_Generic.anyProxy);
          }, function ($dollarq1) {
              return Data_Generic.toSignature(Data_Generic.genericString)(Data_Generic.anyProxy);
          } ]
      } ]);
  }, function (v) {
      if (v instanceof LoginPage) {
          return new Data_Generic.SProd("App.GUI.State.LoginPage", [  ]);
      };
      if (v instanceof PhotoboothsPage) {
          return new Data_Generic.SProd("App.GUI.State.PhotoboothsPage", [  ]);
      };
      if (v instanceof EventsPage) {
          return new Data_Generic.SProd("App.GUI.State.EventsPage", [ function ($dollarq) {
              return Data_Generic.toSpine(Data_Generic.genericString)(v.value0);
          }, function ($dollarq) {
              return Data_Generic.toSpine(Data_Generic.genericString)(v.value1);
          }, function ($dollarq) {
              return Data_Generic.toSpine(Data_Generic.genericInt)(v.value2);
          } ]);
      };
      if (v instanceof StatisticsPage) {
          return new Data_Generic.SProd("App.GUI.State.StatisticsPage", [ function ($dollarq) {
              return Data_Generic.toSpine(Data_Generic.genericString)(v.value0);
          }, function ($dollarq) {
              return Data_Generic.toSpine(Data_Generic.genericString)(v.value1);
          } ]);
      };
      throw new Error("Failed pattern match at App.GUI.State line 83, column 1 - line 87, column 1: " + [ v.constructor.name ]);
  });
  var _username = function (dictStrong) {
      return Data_Lens_Lens.lens(function (v) {
          return v.username;
      })(function (v) {
          return function (v1) {
              var $187 = {};
              for (var $188 in v) {
                  if (v.hasOwnProperty($188)) {
                      $187[$188] = v[$188];
                  };
              };
              $187.username = v1;
              return $187;
          };
      })(dictStrong);
  };
  var _statisticsPage = function (dictStrong) {
      return Data_Lens_Lens.lens(function (v) {
          return v.statisticsPage;
      })(function (v) {
          return function (v1) {
              var $189 = {};
              for (var $190 in v) {
                  if (v.hasOwnProperty($190)) {
                      $189[$190] = v[$190];
                  };
              };
              $189.statisticsPage = v1;
              return $189;
          };
      })(dictStrong);
  };
  var _statistics = function (dictStrong) {
      return Data_Lens_Lens.lens(function (v) {
          return v.statistics;
      })(function (v) {
          return function (v1) {
              var $191 = {};
              for (var $192 in v) {
                  if (v.hasOwnProperty($192)) {
                      $191[$192] = v[$192];
                  };
              };
              $191.statistics = v1;
              return $191;
          };
      })(dictStrong);
  };
  var _state = function (dictStrong) {
      return Data_Lens_Lens.lens(function (v) {
          return v.state;
      })(function (v) {
          return function (v1) {
              var $193 = {};
              for (var $194 in v) {
                  if (v.hasOwnProperty($194)) {
                      $193[$194] = v[$194];
                  };
              };
              $193.state = v1;
              return $193;
          };
      })(dictStrong);
  };
  var _savingFile = function (dictStrong) {
      return Data_Lens_Lens.lens(function (v) {
          return v.savingFile;
      })(function (v) {
          return function (v1) {
              var $197 = {};
              for (var $198 in v) {
                  if (v.hasOwnProperty($198)) {
                      $197[$198] = v[$198];
                  };
              };
              $197.savingFile = v1;
              return $197;
          };
      })(dictStrong);
  };
  var _saving = function (dictStrong) {
      return Data_Lens_Lens.lens(function (v) {
          return v.saving;
      })(function (v) {
          return function (v1) {
              var $199 = {};
              for (var $200 in v) {
                  if (v.hasOwnProperty($200)) {
                      $199[$200] = v[$200];
                  };
              };
              $199.saving = v1;
              return $199;
          };
      })(dictStrong);
  };
  var _route = function (dictStrong) {
      return Data_Lens_Lens.lens(function (v) {
          return v.route;
      })(function (v) {
          return function (v1) {
              var $201 = {};
              for (var $202 in v) {
                  if (v.hasOwnProperty($202)) {
                      $201[$202] = v[$202];
                  };
              };
              $201.route = v1;
              return $201;
          };
      })(dictStrong);
  };
  var _profiles = function (dictStrong) {
      return Data_Lens_Lens.lens(function (v) {
          return v.profiles;
      })(function (v) {
          return function (v1) {
              var $203 = {};
              for (var $204 in v) {
                  if (v.hasOwnProperty($204)) {
                      $203[$204] = v[$204];
                  };
              };
              $203.profiles = v1;
              return $203;
          };
      })(dictStrong);
  };
  var _profile = function (dictStrong) {
      return Data_Lens_Lens.lens(function (v) {
          return v.profile;
      })(function (v) {
          return function (v1) {
              var $205 = {};
              for (var $206 in v) {
                  if (v.hasOwnProperty($206)) {
                      $205[$206] = v[$206];
                  };
              };
              $205.profile = v1;
              return $205;
          };
      })(dictStrong);
  };
  var _pbPage = function (dictStrong) {
      return Data_Lens_Lens.lens(function (obj) {
          return {
              collection: obj.photobooths, 
              profiles: obj.profiles, 
              "new": obj.photoboothsPage["new"], 
              editing: obj.photoboothsPage.editing, 
              deleting: obj.photoboothsPage.deleting
          };
      })(function (old) {
          return function (obj) {
              var $215 = {};
              for (var $216 in old) {
                  if (old.hasOwnProperty($216)) {
                      $215[$216] = old[$216];
                  };
              };
              $215.photobooths = obj.collection;
              $215.profiles = obj.profiles;
              $215.photoboothsPage = {
                  "new": obj["new"], 
                  editing: obj.editing, 
                  deleting: obj.deleting
              };
              return $215;
          };
      })(dictStrong);
  };
  var _password = function (dictStrong) {
      return Data_Lens_Lens.lens(function (v) {
          return v.password;
      })(function (v) {
          return function (v1) {
              var $217 = {};
              for (var $218 in v) {
                  if (v.hasOwnProperty($218)) {
                      $217[$218] = v[$218];
                  };
              };
              $217.password = v1;
              return $217;
          };
      })(dictStrong);
  };
  var _new = function (dictStrong) {
      return Data_Lens_Lens.lens(function (v) {
          return v["new"];
      })(function (v) {
          return function (v1) {
              var $219 = {};
              for (var $220 in v) {
                  if (v.hasOwnProperty($220)) {
                      $219[$220] = v[$220];
                  };
              };
              $219.new = v1;
              return $219;
          };
      })(dictStrong);
  };
  var _name = function (dictStrong) {
      return Data_Lens_Lens.lens(function (v) {
          return v.name;
      })(function (v) {
          return function (v1) {
              var $221 = {};
              for (var $222 in v) {
                  if (v.hasOwnProperty($222)) {
                      $221[$222] = v[$222];
                  };
              };
              $221.name = v1;
              return $221;
          };
      })(dictStrong);
  };
  var _model = function (dictStrong) {
      return Data_Lens_Lens.lens(function (v) {
          return v.model;
      })(function (v) {
          return function (v1) {
              var $227 = {};
              for (var $228 in v) {
                  if (v.hasOwnProperty($228)) {
                      $227[$228] = v[$228];
                  };
              };
              $227.model = v1;
              return $227;
          };
      })(dictStrong);
  };
  var _loginPage = function (dictStrong) {
      return Data_Lens_Lens.lens(function (obj) {
          return {
              session: obj.session, 
              username: obj.loginPage.username, 
              password: obj.loginPage.password, 
              loggingIn: obj.loginPage.loggingIn
          };
      })(function (old) {
          return function (obj) {
              var $229 = {};
              for (var $230 in old) {
                  if (old.hasOwnProperty($230)) {
                      $229[$230] = old[$230];
                  };
              };
              $229.session = obj.session;
              $229.loginPage = {
                  username: obj.username, 
                  password: obj.password, 
                  loggingIn: obj.loggingIn
              };
              return $229;
          };
      })(dictStrong);
  };
  var _loggingIn = function (dictStrong) {
      return Data_Lens_Lens.lens(function (v) {
          return v.loggingIn;
      })(function (v) {
          return function (v1) {
              var $231 = {};
              for (var $232 in v) {
                  if (v.hasOwnProperty($232)) {
                      $231[$232] = v[$232];
                  };
              };
              $231.loggingIn = v1;
              return $231;
          };
      })(dictStrong);
  };
  var _files = function (dictStrong) {
      return Data_Lens_Lens.lens(function (v) {
          return v.files;
      })(function (v) {
          return function (v1) {
              var $237 = {};
              for (var $238 in v) {
                  if (v.hasOwnProperty($238)) {
                      $237[$238] = v[$238];
                  };
              };
              $237.files = v1;
              return $237;
          };
      })(dictStrong);
  };
  var _file = function (dictStrong) {
      return Data_Lens_Lens.lens(function (v) {
          return v.file;
      })(function (v) {
          return function (v1) {
              var $239 = {};
              for (var $240 in v) {
                  if (v.hasOwnProperty($240)) {
                      $239[$240] = v[$240];
                  };
              };
              $239.file = v1;
              return $239;
          };
      })(dictStrong);
  };
  var _eventsPage = function (dictStrong) {
      return Data_Lens_Lens.lens(function (obj) {
          return {
              collection: obj.eventsPage.events, 
              profiles: obj.profiles, 
              "new": obj.eventsPage["new"], 
              editing: obj.eventsPage.editing, 
              deleting: obj.eventsPage.deleting
          };
      })(function (old) {
          return function (obj) {
              var $241 = {};
              for (var $242 in old) {
                  if (old.hasOwnProperty($242)) {
                      $241[$242] = old[$242];
                  };
              };
              $241.profiles = obj.profiles;
              $241.eventsPage = {
                  "new": obj["new"], 
                  editing: obj.editing, 
                  deleting: obj.deleting, 
                  events: obj.collection
              };
              return $241;
          };
      })(dictStrong);
  };
  var _events = function (dictStrong) {
      return Data_Lens_Lens.lens(function (v) {
          return v.events;
      })(function (v) {
          return function (v1) {
              var $243 = {};
              for (var $244 in v) {
                  if (v.hasOwnProperty($244)) {
                      $243[$244] = v[$244];
                  };
              };
              $243.events = v1;
              return $243;
          };
      })(dictStrong);
  };
  var _editing = function (dictStrong) {
      return Data_Lens_Lens.lens(function (v) {
          return v.editing;
      })(function (v) {
          return function (v1) {
              var $249 = {};
              for (var $250 in v) {
                  if (v.hasOwnProperty($250)) {
                      $249[$250] = v[$250];
                  };
              };
              $249.editing = v1;
              return $249;
          };
      })(dictStrong);
  };
  var _deleting = function (dictStrong) {
      return Data_Lens_Lens.lens(function (v) {
          return v.deleting;
      })(function (v) {
          return function (v1) {
              var $251 = {};
              for (var $252 in v) {
                  if (v.hasOwnProperty($252)) {
                      $251[$252] = v[$252];
                  };
              };
              $251.deleting = v1;
              return $251;
          };
      })(dictStrong);
  };
  var _defaultprofile = function (dictStrong) {
      return Data_Lens_Lens.lens(function (v) {
          return v.defaultprofile;
      })(function (v) {
          return function (v1) {
              var $253 = {};
              for (var $254 in v) {
                  if (v.hasOwnProperty($254)) {
                      $253[$254] = v[$254];
                  };
              };
              $253.defaultprofile = v1;
              return $253;
          };
      })(dictStrong);
  };
  var _dateuntil = function (dictStrong) {
      return Data_Lens_Lens.lens(function (v) {
          return v.dateuntil;
      })(function (v) {
          return function (v1) {
              var $255 = {};
              for (var $256 in v) {
                  if (v.hasOwnProperty($256)) {
                      $255[$256] = v[$256];
                  };
              };
              $255.dateuntil = v1;
              return $255;
          };
      })(dictStrong);
  };
  var _datefrom = function (dictStrong) {
      return Data_Lens_Lens.lens(function (v) {
          return v.datefrom;
      })(function (v) {
          return function (v1) {
              var $257 = {};
              for (var $258 in v) {
                  if (v.hasOwnProperty($258)) {
                      $257[$258] = v[$258];
                  };
              };
              $257.datefrom = v1;
              return $257;
          };
      })(dictStrong);
  };
  var _computername = function (dictStrong) {
      return Data_Lens_Lens.lens(function (v) {
          return v.computername;
      })(function (v) {
          return function (v1) {
              var $259 = {};
              for (var $260 in v) {
                  if (v.hasOwnProperty($260)) {
                      $259[$260] = v[$260];
                  };
              };
              $259.computername = v1;
              return $259;
          };
      })(dictStrong);
  };
  var _collectionEditingD = function (dictStrong) {
      return Data_Lens_Lens.lens(function (v) {
          return {
              collection: v.collection, 
              editing: v.editing, 
              deleting: v.deleting
          };
      })(function (old) {
          return function (v) {
              var $266 = {};
              for (var $267 in old) {
                  if (old.hasOwnProperty($267)) {
                      $266[$267] = old[$267];
                  };
              };
              $266.collection = v.collection;
              $266.editing = v.editing;
              $266.deleting = v.deleting;
              return $266;
          };
      })(dictStrong);
  };
  var _collectionEditing = function (dictStrong) {
      return Data_Lens_Lens.lens(function (v) {
          return {
              collection: v.collection, 
              editing: v.editing
          };
      })(function (old) {
          return function (v) {
              var $275 = {};
              for (var $276 in old) {
                  if (old.hasOwnProperty($276)) {
                      $275[$276] = old[$276];
                  };
              };
              $275.collection = v.collection;
              $275.editing = v.editing;
              return $275;
          };
      })(dictStrong);
  };
  var _collection = function (dictStrong) {
      return Data_Lens_Lens.lens(function (v) {
          return v.collection;
      })(function (v) {
          return function (v1) {
              var $279 = {};
              for (var $280 in v) {
                  if (v.hasOwnProperty($280)) {
                      $279[$280] = v[$280];
                  };
              };
              $279.collection = v1;
              return $279;
          };
      })(dictStrong);
  };
  var _alias = function (dictStrong) {
      return Data_Lens_Lens.lens(function (v) {
          return v.alias;
      })(function (v) {
          return function (v1) {
              var $281 = {};
              for (var $282 in v) {
                  if (v.hasOwnProperty($282)) {
                      $281[$282] = v[$282];
                  };
              };
              $281.alias = v1;
              return $281;
          };
      })(dictStrong);
  };
  exports["LoginPage"] = LoginPage;
  exports["PhotoboothsPage"] = PhotoboothsPage;
  exports["EventsPage"] = EventsPage;
  exports["StatisticsPage"] = StatisticsPage;
  exports["_events"] = _events;
  exports["_statistics"] = _statistics;
  exports["_savingFile"] = _savingFile;
  exports["_profiles"] = _profiles;
  exports["_file"] = _file;
  exports["_files"] = _files;
  exports["_defaultprofile"] = _defaultprofile;
  exports["_alias"] = _alias;
  exports["_computername"] = _computername;
  exports["_profile"] = _profile;
  exports["_dateuntil"] = _dateuntil;
  exports["_datefrom"] = _datefrom;
  exports["_name"] = _name;
  exports["_statisticsPage"] = _statisticsPage;
  exports["_eventsPage"] = _eventsPage;
  exports["_pbPage"] = _pbPage;
  exports["_loginPage"] = _loginPage;
  exports["_route"] = _route;
  exports["_saving"] = _saving;
  exports["_deleting"] = _deleting;
  exports["_editing"] = _editing;
  exports["_state"] = _state;
  exports["_loggingIn"] = _loggingIn;
  exports["_password"] = _password;
  exports["_username"] = _username;
  exports["_model"] = _model;
  exports["_new"] = _new;
  exports["_collectionEditingD"] = _collectionEditingD;
  exports["_collectionEditing"] = _collectionEditing;
  exports["_collection"] = _collection;
  exports["initialState"] = initialState;
  exports["genericRoute"] = genericRoute;
})(PS["App.GUI.State"] = PS["App.GUI.State"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Network_HTTP_Affjax = PS["Network.HTTP.Affjax"];
  var Control_Monad_Aff = PS["Control.Monad.Aff"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Tuple = PS["Data.Tuple"];
  var App_Model_Event = PS["App.Model.Event"];
  var App_Model_Statistic = PS["App.Model.Statistic"];
  var App_Model_Async = PS["App.Model.Async"];
  var App_GUI_State = PS["App.GUI.State"];
  var App_Endpoint = PS["App.Endpoint"];
  var Endpoint_Client = PS["Endpoint.Client"];
  var Data_Serializable = PS["Data.Serializable"];
  var Data_Generic = PS["Data.Generic"];        
  var wrapEvent = function (v) {
      return {
          model: v, 
          state: {
              savingFile: App_Model_Async.Initial.value, 
              file: Data_Maybe.Nothing.value
          }
      };
  };
  var loadStatistics = function (s) {
      return Endpoint_Client.execEndpoint(Data_Serializable.serializableString)(Data_Generic.genericUnit)(App_Model_Statistic.genericAllStatistics)(App_Endpoint.getStatistics)(s)(Prelude.unit);
  };
  var loadEventsWithState = function (s) {
      return function (page) {
          return Prelude["<$>"](Control_Monad_Aff.functorAff)(Prelude["<$>"](Prelude.functorFn)(Prelude.map(Prelude.functorArray)(wrapEvent))(App_Model_Event.sortEvents))(Endpoint_Client.execEndpoint(Data_Serializable.serializableTuple(Data_Serializable.serializableString)(Data_Serializable.serializableInt))(Data_Generic.genericUnit)(Data_Generic.genericArray(App_Model_Event.genericEvent))(App_Endpoint.getEventsPaged)(new Data_Tuple.Tuple(s, page))(Prelude.unit));
      };
  };
  var loadEvents = function (s) {
      return Endpoint_Client.execEndpoint(Data_Serializable.serializableString)(Data_Generic.genericUnit)(Data_Generic.genericArray(App_Model_Event.genericEvent))(App_Endpoint.getEvents)(s)(Prelude.unit);
  };
  exports["loadStatistics"] = loadStatistics;
  exports["wrapEvent"] = wrapEvent;
  exports["loadEventsWithState"] = loadEventsWithState;
  exports["loadEvents"] = loadEvents;
})(PS["App.GUI.Load"] = PS["App.GUI.Load"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Control_Monad_State_Class = PS["Control.Monad.State.Class"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Tuple = PS["Data.Tuple"];
  var Data_Lens_Types_1 = PS["Data.Lens.Types"];
  var Data_Lens_Types_1 = PS["Data.Lens.Types"];
  var Data_Lens_Internal_Indexed = PS["Data.Lens.Internal.Indexed"];        
  var over = function (l) {
      return l;
  };
  var set = function (l) {
      return function (b) {
          return over(l)(Prelude["const"](b));
      };
  };
  exports["set"] = set;
  exports["over"] = over;
})(PS["Data.Lens.Setter"] = PS["Data.Lens.Setter"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["App.GUI.Router"];
  var App_GUI_Load = PS["App.GUI.Load"];
  var App_GUI_State = PS["App.GUI.State"];
  var App_GUI_Types = PS["App.GUI.Types"];
  var App_Model_Async = PS["App.Model.Async"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Control_Monad_Eff_Ref = PS["Control.Monad.Eff.Ref"];
  var DOM = PS["DOM"];
  var Data_Either = PS["Data.Either"];
  var Data_Foreign = PS["Data.Foreign"];
  var Data_Foreign_Generic = PS["Data.Foreign.Generic"];
  var Data_Lens = PS["Data.Lens"];
  var Data_String = PS["Data.String"];
  var Network_HTTP_Affjax = PS["Network.HTTP.Affjax"];
  var OpticUI = PS["OpticUI"];
  var OpticUI_Components_Async = PS["OpticUI.Components.Async"];
  var Prelude = PS["Prelude"];
  var Data_Lens_Setter = PS["Data.Lens.Setter"];
  var Data_Profunctor_Strong = PS["Data.Profunctor.Strong"];
  var OpticUI_Core = PS["OpticUI.Core"];        
  var toUrl = Data_Foreign_Generic.toJSONGeneric(App_GUI_State.genericRoute)(Data_Foreign_Generic.defaultOptions);
  var resolve = function (s) {
      return function (v) {
          if (v instanceof App_GUI_State.PhotoboothsPage) {
              return Prelude["return"](Control_Monad_Eff.applicativeEff)(s);
          };
          if (v instanceof App_GUI_State.EventsPage) {
              return function __do() {
                  var v1 = OpticUI_Components_Async.async(App_GUI_Load.loadEventsWithState(v.value0)(v.value2))();
                  var modifications = function ($14) {
                      return Data_Lens_Setter.set(function ($15) {
                          return App_GUI_State._eventsPage(Data_Profunctor_Strong.strongFn)(App_GUI_State._new(Data_Profunctor_Strong.strongFn)(App_GUI_State._model(Data_Profunctor_Strong.strongFn)(App_GUI_State._computername(Data_Profunctor_Strong.strongFn)($15))));
                      })(v.value0)(Data_Lens_Setter.set(function ($16) {
                          return App_GUI_State._eventsPage(Data_Profunctor_Strong.strongFn)(App_GUI_State._collection(Data_Profunctor_Strong.strongFn)($16));
                      })(new App_Model_Async.Busy(v1))($14));
                  };
                  return modifications(s);
              };
          };
          if (v instanceof App_GUI_State.StatisticsPage) {
              return function __do() {
                  var v1 = OpticUI_Components_Async.async(App_GUI_Load.loadEvents(v.value0))();
                  var v2 = OpticUI_Components_Async.async(App_GUI_Load.loadStatistics(v.value0))();
                  var modifications = function ($17) {
                      return Data_Lens_Setter.set(function ($18) {
                          return App_GUI_State._statisticsPage(Data_Profunctor_Strong.strongFn)(App_GUI_State._events(Data_Profunctor_Strong.strongFn)($18));
                      })(new App_Model_Async.Busy(v1))(Data_Lens_Setter.set(function ($19) {
                          return App_GUI_State._statisticsPage(Data_Profunctor_Strong.strongFn)(App_GUI_State._statistics(Data_Profunctor_Strong.strongFn)($19));
                      })(new App_Model_Async.Busy(v2))($17));
                  };
                  return modifications(s);
              };
          };
          throw new Error("Failed pattern match at App.GUI.Router line 39, column 1 - line 40, column 1: " + [ s.constructor.name, v.constructor.name ]);
      };
  };
  var nav = function (s) {
      return function (h) {
          return function (r) {
              return function __do() {
                  var $20 = Control_Apply["*>"](Control_Monad_Eff.applyEff)($foreign.setHash(toUrl(r)))(resolve(s)(r))();
                  return OpticUI_Core.runHandler(h)(Data_Lens_Setter.set(App_GUI_State._route(Data_Profunctor_Strong.strongFn))(r)($20))();
              };
          };
      };
  };
  var fromUrl = Data_Foreign_Generic.readJSONGeneric(App_GUI_State.genericRoute)(Data_Foreign_Generic.defaultOptions);
  var match = function (str) {
      return Data_Either.either(Prelude["const"](App_GUI_State.PhotoboothsPage.value))(Prelude.id(Prelude.categoryFn))(fromUrl(Data_String.drop(1)(str)));
  };
  exports["fromUrl"] = fromUrl;
  exports["toUrl"] = toUrl;
  exports["match"] = match;
  exports["resolve"] = resolve;
  exports["nav"] = nav;
  exports["hashChanged"] = $foreign.hashChanged;
  exports["getHash"] = $foreign.getHash;
})(PS["App.GUI.Router"] = PS["App.GUI.Router"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  // module Data.Exists

  exports.mkExists = function (fa) {
    return fa;
  };

  exports.runExists = function (f) {
    return function (fa) {
      return f(fa);
    };
  };
})(PS["Data.Exists"] = PS["Data.Exists"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["Data.Exists"];
  var Prelude = PS["Prelude"];
  exports["runExists"] = $foreign.runExists;
  exports["mkExists"] = $foreign.mkExists;
})(PS["Data.Exists"] = PS["Data.Exists"] || {});
(function(exports) {
  /* global exports, require */
  "use strict";

  // module OpticUI.Internal.VirtualDOM

  // VTree -> HTMLElement
  exports.createElement = function () {
    var vcreateElement = require('virtual-dom/create-element');
    return function (vtree) {
      return vcreateElement(vtree);
    };
  }();

  // VTree -> VTree -> Patch
  exports.diff = function () {
    var vdiff = require('virtual-dom/diff');
    return function (a) {
      return function (b) {
        return vdiff(a, b);
      };
    };
  }();

  // forall eff. Patch -> HTMLElement -> Eff (dom :: DOM | eff) HTMLElement
  exports.patch = function () {
    var vpatch = require('virtual-dom/patch');
    return function (p) {
      return function (node) {
        return function () {
          return vpatch(node, p);
        };
      };
    };
  }();

  // String -> VTree
  exports.vtext = function () {
    var VText = require('virtual-dom/vnode/vtext');
    return function (s) { return new VText(s); };
  }();

  // Nullable String -> String -> Nullable String -> Props -> Array VTree -> VTree
  exports.vnode = function () {
    var VirtualNode = require("virtual-dom/vnode/vnode");
    var SoftSetHook = require("virtual-dom/virtual-hyperscript/hooks/soft-set-hook");
    return function (ns) {
      return function (name) {
        return function (key) {
          return function (props) {
            return function (children) {
              if (name === "input" && props.value !== undefined) {
                props.value = new SoftSetHook(props.value);
              }
              return new VirtualNode(name, props, children, key, ns);
            };
          };
        };
      };
    };
  }();

  // forall a. Fn2 String a Props
  exports.prop = function (key, value) {
    var props = {};
    props[key] = value;
    return props;
  };

  // Fn2 String String Props
  exports.attrProp = function (key, value) {
    var props = { attributes : {} };
    props.attributes[key] = value;
    return props;
  };

  // forall eff e. Fn2 String (e -> Eff eff Unit) Props
  exports.handlerProp = function (key, f) {
    var Hook  = function () {};
    var props = {};
    Hook.prototype.callback = function (e) {
      f(e)();
    };
    Hook.prototype.hook = function (node) {
      node.addEventListener(key, this.callback);
    };
    Hook.prototype.unhook = function (node) {
      node.removeEventListener(key, this.callback);
    };
    props["opticui-hook-" + key] = new Hook(f);
    return props;
  };

  // Fn2 Props Props Props
  exports.concatProps = function () {
    var hOP = Object.prototype.hasOwnProperty;
    var copy = function (source, result) {
      for (var key in source) {
        if (hOP.call(source, key)) {
          if (key === "attributes") {
            var sourceAttrs = source[key];
            var resultAttrs = result[key];

            for (var attr in sourceAttrs) {
              if (hOP.call(sourceAttrs, attr)) {
                resultAttrs[attr] = sourceAttrs[attr];
              }
            }
          } else {
            result[key] = source[key];
          }
        }
      }
      return result;
    };
    return function (p, q) {
      return copy(p, copy(q, { attributes: {} } ));
    };
  }();

  // Props
  exports.emptyProps = {};

  //Fn2 String (HTMLElement -> Eff eff Unit) Props
  exports.initializer = function(s, f){
    var Hook = function () {};
    Hook.prototype.hook = function (node) {
      if(!node.initialized){
        node.initialized = true;
        f(node)();
      }
    };
    var obj = {};
    obj[s] = new Hook(f);
    return obj;
  };

  //Fn2 String (HTMLElement -> Eff eff Unit) Props
  exports.finalizer = function(s, f){
    var Hook = function () {};
    Hook.prototype.unhook = function (node) {
      f(node)();
    };
    var obj = {};
    obj[s] = new Hook(f);
    return obj;
  };
})(PS["OpticUI.Internal.VirtualDOM"] = PS["OpticUI.Internal.VirtualDOM"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["DOM.HTML.Types"];
  var Prelude = PS["Prelude"];
  var Data_Either = PS["Data.Either"];
  var Data_Foreign = PS["Data.Foreign"];
  var Data_Foreign_Class = PS["Data.Foreign.Class"];
  var DOM_Event_Types = PS["DOM.Event.Types"];
  var DOM_Node_Types = PS["DOM.Node.Types"];
  var Unsafe_Coerce = PS["Unsafe.Coerce"];        
  var windowToEventTarget = Unsafe_Coerce.unsafeCoerce;                  
  var htmlElementToNode = Unsafe_Coerce.unsafeCoerce;
  exports["htmlElementToNode"] = htmlElementToNode;
  exports["windowToEventTarget"] = windowToEventTarget;
})(PS["DOM.HTML.Types"] = PS["DOM.HTML.Types"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["OpticUI.Internal.VirtualDOM"];
  var Prelude = PS["Prelude"];
  var Data_Monoid = PS["Data.Monoid"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Data_Function = PS["Data.Function"];
  var DOM = PS["DOM"];
  var DOM_HTML_Types = PS["DOM.HTML.Types"];
  var Data_Nullable = PS["Data.Nullable"];        
  var semigroupProps = new Prelude.Semigroup(Data_Function.runFn2($foreign.concatProps));
  var monoidProps = new Data_Monoid.Monoid(function () {
      return semigroupProps;
  }, $foreign.emptyProps);
  exports["semigroupProps"] = semigroupProps;
  exports["monoidProps"] = monoidProps;
  exports["finalizer"] = $foreign.finalizer;
  exports["initializer"] = $foreign.initializer;
  exports["handlerProp"] = $foreign.handlerProp;
  exports["attrProp"] = $foreign.attrProp;
  exports["prop"] = $foreign.prop;
  exports["vnode"] = $foreign.vnode;
  exports["vtext"] = $foreign.vtext;
  exports["patch"] = $foreign.patch;
  exports["diff"] = $foreign.diff;
  exports["createElement"] = $foreign.createElement;
})(PS["OpticUI.Internal.VirtualDOM"] = PS["OpticUI.Internal.VirtualDOM"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Exists = PS["Data.Exists"];
  var OpticUI_Internal_VirtualDOM = PS["OpticUI.Internal.VirtualDOM"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_Foldable = PS["Data.Foldable"];
  var Data_Nullable = PS["Data.Nullable"];
  var Data_Function = PS["Data.Function"];
  var DOM_HTML_Types = PS["DOM.HTML.Types"];
  var Control_Monad_Writer_Trans = PS["Control.Monad.Writer.Trans"];
  var Unsafe_Coerce = PS["Unsafe.Coerce"];        
  var PropE = (function () {
      function PropE(value0) {
          this.value0 = value0;
      };
      PropE.create = function (value0) {
          return new PropE(value0);
      };
      return PropE;
  })();
  var AttrP = (function () {
      function AttrP(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      AttrP.create = function (value0) {
          return function (value1) {
              return new AttrP(value0, value1);
          };
      };
      return AttrP;
  })();
  var PropP = (function () {
      function PropP(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      PropP.create = function (value0) {
          return function (value1) {
              return new PropP(value0, value1);
          };
      };
      return PropP;
  })();
  var HandlerP = (function () {
      function HandlerP(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      HandlerP.create = function (value0) {
          return function (value1) {
              return new HandlerP(value0, value1);
          };
      };
      return HandlerP;
  })();
  var InitializerP = (function () {
      function InitializerP(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      InitializerP.create = function (value0) {
          return function (value1) {
              return new InitializerP(value0, value1);
          };
      };
      return InitializerP;
  })();
  var FinalizerP = (function () {
      function FinalizerP(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      FinalizerP.create = function (value0) {
          return function (value1) {
              return new FinalizerP(value0, value1);
          };
      };
      return FinalizerP;
  })();
  var Text = (function () {
      function Text(value0) {
          this.value0 = value0;
      };
      Text.create = function (value0) {
          return new Text(value0);
      };
      return Text;
  })();
  var Element = (function () {
      function Element(value0, value1, value2, value3) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value2;
          this.value3 = value3;
      };
      Element.create = function (value0) {
          return function (value1) {
              return function (value2) {
                  return function (value3) {
                      return new Element(value0, value1, value2, value3);
                  };
              };
          };
      };
      return Element;
  })();
  var Markup = (function () {
      function Markup(value0) {
          this.value0 = value0;
      };
      Markup.create = function (value0) {
          return new Markup(value0);
      };
      return Markup;
  })();
  var text = function (t) {
      return new Markup([ new Text(t) ]);
  };
  var runInitializer = function (f) {
      return function (init) {
          return f(Unsafe_Coerce.unsafeCoerce(init));
      };
  };
  var runFinalizer = function (f) {
      return function (i) {
          return f(Unsafe_Coerce.unsafeCoerce(i));
      };
  };
  var runEventHandler = function (f) {
      return function (h) {
          return f(Unsafe_Coerce.unsafeCoerce(h));
      };
  };
  var prop = function (n) {
      return function (v) {
          return new PropP(n, Data_Exists.mkExists(new PropE(v)));
      };
  };                                           
  var mkEventHandler = Unsafe_Coerce.unsafeCoerce;
  var markupSemigroup = new Prelude.Semigroup(function (v) {
      return function (v1) {
          return new Markup(Prelude["++"](Prelude.semigroupArray)(v.value0)(v1.value0));
      };
  });
  var markupMonoid = new Data_Monoid.Monoid(function () {
      return markupSemigroup;
  }, new Markup([  ]));
  var handle = function (n) {
      return function (f) {
          return new HandlerP(n, mkEventHandler(f));
      };
  };
  var element = function (ns) {
      return function (tag) {
          return function (props) {
              return function (childs) {
                  return new Markup([ new Element(ns, tag, props, childs) ]);
              };
          };
      };
  };
  var attr = function (n) {
      return function (v) {
          return new AttrP(n, v);
      };
  };
  exports["PropE"] = PropE;
  exports["AttrP"] = AttrP;
  exports["PropP"] = PropP;
  exports["HandlerP"] = HandlerP;
  exports["InitializerP"] = InitializerP;
  exports["FinalizerP"] = FinalizerP;
  exports["Text"] = Text;
  exports["Element"] = Element;
  exports["Markup"] = Markup;
  exports["runFinalizer"] = runFinalizer;
  exports["runInitializer"] = runInitializer;
  exports["runEventHandler"] = runEventHandler;
  exports["mkEventHandler"] = mkEventHandler;
  exports["handle"] = handle;
  exports["prop"] = prop;
  exports["attr"] = attr;
  exports["text"] = text;
  exports["element"] = element;
  exports["markupSemigroup"] = markupSemigroup;
  exports["markupMonoid"] = markupMonoid;
})(PS["OpticUI.Markup"] = PS["OpticUI.Markup"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var OpticUI_Markup = PS["OpticUI.Markup"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Either = PS["Data.Either"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_Foreign = PS["Data.Foreign"];
  var Data_Foreign_Class = PS["Data.Foreign.Class"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var DOM_HTML_Types = PS["DOM.HTML.Types"];
  var Data_Foreign_Index = PS["Data.Foreign.Index"];                  
  var valueA = OpticUI_Markup.prop("value");                    
  var typeA = OpticUI_Markup.attr("type");                              
  var tr = OpticUI_Markup.element(Data_Maybe.Nothing.value)("tr");      
  var th = OpticUI_Markup.element(Data_Maybe.Nothing.value)("th");            
  var td = OpticUI_Markup.element(Data_Maybe.Nothing.value)("td");
  var tbody = OpticUI_Markup.element(Data_Maybe.Nothing.value)("tbody");
  var table = OpticUI_Markup.element(Data_Maybe.Nothing.value)("table");
  var select = OpticUI_Markup.element(Data_Maybe.Nothing.value)("select");
  var option = OpticUI_Markup.element(Data_Maybe.Nothing.value)("option");
  var onClick = function (h) {
      return OpticUI_Markup.handle("click")(h);
  };                                                                      
  var label = OpticUI_Markup.element(Data_Maybe.Nothing.value)("label");
  var input_ = function (ps) {
      return OpticUI_Markup.element(Data_Maybe.Nothing.value)("input")(ps)(Data_Monoid.mempty(OpticUI_Markup.markupMonoid));
  };                                                                     
  var h1 = OpticUI_Markup.element(Data_Maybe.Nothing.value)("h1");
  var getProp = function (dictIsForeign) {
      return function (p1) {
          return function ($4) {
              return Data_Either.either(Prelude["const"](Data_Maybe.Nothing.value))(Data_Maybe.Just.create)(Data_Foreign_Class.readProp(dictIsForeign)(Data_Foreign_Index.indexString)(p1)(Data_Foreign.toForeign((function (v) {
                  return v.target;
              })($4))));
          };
      };
  };
  var onInput = function (dictIsForeign) {
      return function (h) {
          return OpticUI_Markup.handle("input")(function (e) {
              return h(e)(getProp(dictIsForeign)("value")(e));
          });
      };
  };                                                                    
  var em = OpticUI_Markup.element(Data_Maybe.Nothing.value)("em");         
  var div = OpticUI_Markup.element(Data_Maybe.Nothing.value)("div");  
  var classA = OpticUI_Markup.attr("class");                              
  var button = OpticUI_Markup.element(Data_Maybe.Nothing.value)("button");
  var br = OpticUI_Markup.element(Data_Maybe.Nothing.value)("br");
  exports["getProp"] = getProp;
  exports["onInput"] = onInput;
  exports["onClick"] = onClick;
  exports["valueA"] = valueA;
  exports["typeA"] = typeA;
  exports["classA"] = classA;
  exports["tr"] = tr;
  exports["th"] = th;
  exports["td"] = td;
  exports["tbody"] = tbody;
  exports["table"] = table;
  exports["select"] = select;
  exports["option"] = option;
  exports["label"] = label;
  exports["input_"] = input_;
  exports["h1"] = h1;
  exports["em"] = em;
  exports["div"] = div;
  exports["button"] = button;
  exports["br"] = br;
})(PS["OpticUI.Markup.HTML"] = PS["OpticUI.Markup.HTML"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var OpticUI_Core = PS["OpticUI.Core"];
  var OpticUI_Markup = PS["OpticUI.Markup"];
  var OpticUI_Markup_HTML = PS["OpticUI.Markup.HTML"];
  var DOM = PS["DOM"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Foreign_Class = PS["Data.Foreign.Class"];        
  var textField = function (as) {
      return OpticUI_Core["with"](function (s) {
          return function (h) {
              var inpH = function (v) {
                  return function ($2) {
                      return OpticUI_Core.runHandler(h)(Data_Maybe.maybe("")(Prelude.id(Prelude.categoryFn))($2));
                  };
              };
              var bs = [ OpticUI_Markup_HTML.valueA(s), OpticUI_Markup_HTML.typeA("text"), OpticUI_Markup_HTML.onInput(Data_Foreign_Class.stringIsForeign)(inpH) ];
              return OpticUI_Core.ui(OpticUI_Markup_HTML.input_(Prelude["++"](Prelude.semigroupArray)(as)(bs)));
          };
      });
  };
  exports["textField"] = textField;
})(PS["OpticUI.Components"] = PS["OpticUI.Components"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Control_Alternative = PS["Control.Alternative"];
  var Control_Plus = PS["Control.Plus"];
  var Data_Monoid_Disj = PS["Data.Monoid.Disj"];
  var Data_Profunctor_Star = PS["Data.Profunctor.Star"];
  var Data_Traversable = PS["Data.Traversable"];
  var Data_Tuple = PS["Data.Tuple"];
  var Data_Lens_Types_2 = PS["Data.Lens.Types"];
  var Data_Lens_Types_2 = PS["Data.Lens.Types"];
  var Data_Lens_Types_2 = PS["Data.Lens.Types"];
  var Data_Lens_Indexed = PS["Data.Lens.Indexed"];
  var Data_Lens_Internal_Wander = PS["Data.Lens.Internal.Wander"];
  var Data_Lens_Internal_Indexed = PS["Data.Lens.Internal.Indexed"];        
  var traversed = function (dictTraversable) {
      return function (dictWander) {
          return Data_Lens_Internal_Wander.wander(dictWander)(function (dictApplicative) {
              return Data_Traversable.traverse(dictTraversable)(dictApplicative);
          });
      };
  };
  exports["traversed"] = traversed;
})(PS["Data.Lens.Traversal"] = PS["Data.Lens.Traversal"] || {});
(function(exports) {
  /* global exports */
  "use strict";


  // module DOM.Timer

  exports.timeout = function(time){
    return function(fn){
      return function(){
        return setTimeout(function(){
          fn();
        }, time);
      };
    };
  };
})(PS["DOM.Timer"] = PS["DOM.Timer"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["DOM.Timer"];
  var Prelude = PS["Prelude"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  exports["timeout"] = $foreign.timeout;
})(PS["DOM.Timer"] = PS["DOM.Timer"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var OpticUI = PS["OpticUI"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var DOM_Timer = PS["DOM.Timer"];
  var Data_Monoid = PS["Data.Monoid"];
  var OpticUI_Core = PS["OpticUI.Core"];        
  var exec = function (dictMonoid) {
      return function (eff) {
          return OpticUI_Core.inline(Control_Apply["*>"](Control_Monad_Eff.applyEff)(DOM_Timer.timeout(0)(eff))(Prelude["return"](Control_Monad_Eff.applicativeEff)(Data_Monoid.mempty(dictMonoid))));
      };
  };
  exports["exec"] = exec;
})(PS["App.GUI.Components.Exec"] = PS["App.GUI.Components.Exec"] || {});
(function(exports) {
    "use strict";

  //module App.GUI.Components.FileInput

  exports.name = function name(f){
    return f.name;
  };

  exports.firstFileImpl = function firstFileImpl(nothing){
    return function(just){
      return function firstFile(ev){
        var f, files = ev.target.files;
        if(files){
          f = files[0];
          return f ? just(f) : nothing;
        } else {
          return nothing;
        }
      };
    };
  };
})(PS["App.GUI.Components.FileInput"] = PS["App.GUI.Components.FileInput"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["App.GUI.Components.FileInput"];
  var Prelude = PS["Prelude"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var OpticUI = PS["OpticUI"];
  var OpticUI_Markup = PS["OpticUI.Markup"];
  var OpticUI_Markup_HTML = PS["OpticUI.Markup.HTML"];
  var DOM = PS["DOM"];
  var DOM_File_Types = PS["DOM.File.Types"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_Array = PS["Data.Array"];
  var Data_Foreign_Class = PS["Data.Foreign.Class"];
  var Unsafe_Coerce = PS["Unsafe.Coerce"];
  var App_GUI_Types = PS["App.GUI.Types"];
  var OpticUI_Core = PS["OpticUI.Core"];        
  var firstFile = $foreign.firstFileImpl(Data_Maybe.Nothing.value)(Data_Maybe.Just.create);
  var onFileInput = function (h) {
      return OpticUI_Markup.handle("change")(function (e) {
          return h(e)(firstFile(e));
      });
  };
  var fileInput = function (ps) {
      return OpticUI_Core["with"](function (s) {
          return function (h) {
              var props = Prelude["<>"](Prelude.semigroupArray)([ OpticUI_Markup_HTML.typeA("file"), onFileInput(function (v) {
                  return function (f) {
                      return OpticUI_Core.runHandler(h)(f);
                  };
              }) ])((function () {
                  var $2 = Data_Maybe.isNothing(s);
                  if ($2) {
                      return [ OpticUI_Markup_HTML.valueA("") ];
                  };
                  if (!$2) {
                      return Data_Monoid.mempty(Data_Monoid.monoidArray);
                  };
                  throw new Error("Failed pattern match at App.GUI.Components.FileInput line 32, column 20 - line 32, column 65: " + [ $2.constructor.name ]);
              })());
              return OpticUI_Core.ui(OpticUI_Markup_HTML.input_(Prelude["<>"](Prelude.semigroupArray)(ps)(props)));
          };
      });
  };
  exports["onFileInput"] = onFileInput;
  exports["fileInput"] = fileInput;
  exports["name"] = $foreign.name;
})(PS["App.GUI.Components.FileInput"] = PS["App.GUI.Components.FileInput"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var OpticUI = PS["OpticUI"];
  var OpticUI_Markup = PS["OpticUI.Markup"];
  var OpticUI_Markup_HTML = PS["OpticUI.Markup.HTML"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Foldable = PS["Data.Foldable"];
  var OpticUI_Core = PS["OpticUI.Core"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Data_Foreign_Class = PS["Data.Foreign.Class"];        
  var selected = OpticUI_Markup.prop("selected");
  var select = function (os) {
      return function (toStr) {
          return function (ps) {
              return OpticUI_Core["with"](function (s) {
                  return function (h) {
                      var sToStr = toStr(s);
                      var options = (function () {
                          var $2 = Data_Foldable.find(Data_Foldable.foldableArray)(function (o) {
                              return toStr(o) === sToStr;
                          })(os);
                          if ($2 instanceof Data_Maybe.Nothing) {
                              return Prelude["<>"](Prelude.semigroupArray)(Prelude.map(Prelude.functorArray)(toStr)(os))([ sToStr ]);
                          };
                          if ($2 instanceof Data_Maybe.Just) {
                              return Prelude.map(Prelude.functorArray)(toStr)(os);
                          };
                          throw new Error("Failed pattern match at App.GUI.Components.Select line 17, column 17 - line 20, column 7: " + [ $2.constructor.name ]);
                      })();
                      var handler = function (v) {
                          return function (v1) {
                              if (v1 instanceof Data_Maybe.Just) {
                                  return Data_Maybe.maybe(Prelude["return"](Control_Monad_Eff.applicativeEff)(Prelude.unit))(function (o) {
                                      return OpticUI_Core.runHandler(h)(o);
                                  })(Data_Foldable.find(Data_Foldable.foldableArray)(function (o) {
                                      return toStr(o) === v1.value0;
                                  })(os));
                              };
                              if (v1 instanceof Data_Maybe.Nothing) {
                                  return Prelude["return"](Control_Monad_Eff.applicativeEff)(Prelude.unit);
                              };
                              throw new Error("Failed pattern match at App.GUI.Components.Select line 14, column 3 - line 23, column 1: " + [ v.constructor.name, v1.constructor.name ]);
                          };
                      };
                      var props = Prelude["++"](Prelude.semigroupArray)(ps)([ OpticUI_Markup_HTML.onInput(Data_Foreign_Class.stringIsForeign)(handler) ]);
                      return OpticUI_Core.ui(OpticUI_Markup_HTML.select(props)(Data_Foldable.foldMap(Data_Foldable.foldableArray)(OpticUI_Markup.markupMonoid)(function (o) {
                          return OpticUI_Markup_HTML.option([ OpticUI_Markup_HTML.valueA(o), selected(o === sToStr) ])(OpticUI_Markup.text(o));
                      })(options)));
                  };
              });
          };
      };
  };
  exports["selected"] = selected;
  exports["select"] = select;
})(PS["App.GUI.Components.Select"] = PS["App.GUI.Components.Select"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var DOM = PS["DOM"];
  var OpticUI = PS["OpticUI"];
  var OpticUI_Markup = PS["OpticUI.Markup"];
  var OpticUI_Markup_HTML = PS["OpticUI.Markup.HTML"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_String = PS["Data.String"];
  var Data_Date = PS["Data.Date"];
  var App_Model_Date = PS["App.Model.Date"];
  var OpticUI_Core = PS["OpticUI.Core"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Data_Foreign_Class = PS["Data.Foreign.Class"];        
  var dateTimeField = function (ps) {
      return OpticUI_Core["with"](function (s) {
          return function (h) {
              var handler = function (v) {
                  return function (v1) {
                      return Data_Maybe.maybe(function __do() {
                          var d = Data_Date.now();
                          return OpticUI_Core.runHandler(h)(d)();
                      })(function (d) {
                          return OpticUI_Core.runHandler(h)(d);
                      })(App_Model_Date.fromLocalDatetime(Data_Maybe.maybe("")(Prelude.id(Prelude.categoryFn))(v1)));
                  };
              };
              var props = [ OpticUI_Markup_HTML.typeA("datetime-local"), OpticUI_Markup_HTML.onInput(Data_Foreign_Class.stringIsForeign)(handler), OpticUI_Markup_HTML.valueA(Data_String.take(16)(App_Model_Date.toLocalDatetime(s))) ];
              return OpticUI_Core.ui(OpticUI_Markup_HTML.input_(Prelude["++"](Prelude.semigroupArray)(ps)(props)));
          };
      });
  };
  exports["dateTimeField"] = dateTimeField;
})(PS["App.GUI.Components.DateTimeField"] = PS["App.GUI.Components.DateTimeField"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var OpticUI_Markup_HTML = PS["OpticUI.Markup.HTML"];
  var OpticUI_Markup = PS["OpticUI.Markup"];
  var OpticUI_Core = PS["OpticUI.Core"];
  var Data_Foldable = PS["Data.Foldable"];        
  var tdUIs = function (uis) {
      return Data_Foldable.mconcat(Data_Foldable.foldableArray)(OpticUI_Core.uiMonoid(OpticUI_Markup.markupMonoid))(Prelude["<$>"](Prelude.functorArray)(OpticUI_Core.withView(OpticUI_Markup_HTML.td([  ])))(uis));
  };
  var tableHeader = function (dictFunctor) {
      return function (dictFoldable) {
          return function (ps) {
              return function (hs) {
                  return OpticUI_Markup_HTML.tr(ps)(Data_Foldable.mconcat(dictFoldable)(OpticUI_Markup.markupMonoid)(Prelude["<$>"](dictFunctor)(function ($2) {
                      return OpticUI_Markup_HTML.th([  ])(OpticUI_Markup.text($2));
                  })(hs)));
              };
          };
      };
  };
  var rowUI = function ($3) {
      return OpticUI_Core.withView(OpticUI_Markup_HTML.tr([  ]))(tdUIs($3));
  };
  var pageTitle = function (s) {
      return OpticUI_Markup_HTML.h1([ OpticUI_Markup_HTML.classA("page-title") ])(s);
  };
  var emptyTd = OpticUI_Markup_HTML.td([  ])(OpticUI_Markup.text(""));
  var crudTable = function ($4) {
      return OpticUI_Markup_HTML.div([ OpticUI_Markup_HTML.classA("crud-table-wrapper") ])(OpticUI_Markup_HTML.table([ OpticUI_Markup_HTML.classA("table crud-table") ])(OpticUI_Markup_HTML.tbody([  ])($4)));
  };
  exports["rowUI"] = rowUI;
  exports["tdUIs"] = tdUIs;
  exports["emptyTd"] = emptyTd;
  exports["tableHeader"] = tableHeader;
  exports["crudTable"] = crudTable;
  exports["pageTitle"] = pageTitle;
})(PS["App.GUI.Components.Markup"] = PS["App.GUI.Components.Markup"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Data_Tuple = PS["Data.Tuple"];
  var Data_Either = PS["Data.Either"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_Const = PS["Data.Const"];
  var Data_Profunctor = PS["Data.Profunctor"];
  var Data_Profunctor_Strong = PS["Data.Profunctor.Strong"];
  var Data_Profunctor_Choice = PS["Data.Profunctor.Choice"];
  var Data_Profunctor_Cochoice = PS["Data.Profunctor.Cochoice"];
  var Data_Lens_Internal_Wander = PS["Data.Lens.Internal.Wander"];
  var runForget = function (v) {
      return v;
  };
  var profunctorForget = new Data_Profunctor.Profunctor(function (f) {
      return function (v) {
          return function (v1) {
              return function ($24) {
                  return v1(f($24));
              };
          };
      };
  });
  var strongForget = new Data_Profunctor_Strong.Strong(function () {
      return profunctorForget;
  }, function (v) {
      return function ($25) {
          return v(Data_Tuple.fst($25));
      };
  }, function (v) {
      return function ($26) {
          return v(Data_Tuple.snd($26));
      };
  });
  var choiceForget = function (dictMonoid) {
      return new Data_Profunctor_Choice.Choice(function () {
          return profunctorForget;
      }, function (v) {
          return Data_Either.either(v)(Data_Monoid.mempty(Data_Monoid.monoidFn(dictMonoid)));
      }, function (v) {
          return Data_Either.either(Data_Monoid.mempty(Data_Monoid.monoidFn(dictMonoid)))(v);
      });
  };
  exports["runForget"] = runForget;
  exports["profunctorForget"] = profunctorForget;
  exports["choiceForget"] = choiceForget;
  exports["strongForget"] = strongForget;
})(PS["Data.Lens.Internal.Forget"] = PS["Data.Lens.Internal.Forget"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Data_Const = PS["Data.Const"];
  var Data_Functor_Contravariant = PS["Data.Functor.Contravariant"];
  var Data_Profunctor = PS["Data.Profunctor"];
  var Data_Profunctor_Star = PS["Data.Profunctor.Star"];
  var Data_Tuple = PS["Data.Tuple"];
  var Control_Monad_State_Class = PS["Control.Monad.State.Class"];
  var Data_Lens_Internal_Forget = PS["Data.Lens.Internal.Forget"];
  var Data_Lens_Types_2 = PS["Data.Lens.Types"];
  var Data_Lens_Types_2 = PS["Data.Lens.Types"];
  var Data_Lens_Types_2 = PS["Data.Lens.Types"];
  var Data_Lens_Internal_Indexed = PS["Data.Lens.Internal.Indexed"];        
  var view = function (l) {
      return Data_Lens_Internal_Forget.runForget(l(Prelude.id(Prelude.categoryFn)));
  };
  exports["view"] = view;
})(PS["Data.Lens.Getter"] = PS["Data.Lens.Getter"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Data_Either = PS["Data.Either"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Lens_Prism = PS["Data.Lens.Prism"];
  var _Just = function (dictChoice) {
      return Data_Lens_Prism.prism(Data_Maybe.Just.create)(Data_Maybe.maybe(new Data_Either.Left(Data_Maybe.Nothing.value))(Data_Either.Right.create))(dictChoice);
  };
  exports["_Just"] = _Just;
})(PS["Data.Lens.Prism.Maybe"] = PS["Data.Lens.Prism.Maybe"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var Control_Monad_Aff = PS["Control.Monad.Aff"];
  var Control_Monad_Eff_Exception = PS["Control.Monad.Eff.Exception"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Control_Monad_Eff_Ref = PS["Control.Monad.Eff.Ref"];
  var OpticUI = PS["OpticUI"];
  var OpticUI_Components_Async = PS["OpticUI.Components.Async"];
  var Data_Lens = PS["Data.Lens"];
  var Data_Lens_Common = PS["Data.Lens.Common"];
  var Data_Array = PS["Data.Array"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Monoid = PS["Data.Monoid"];
  var App_GUI_Types = PS["App.GUI.Types"];
  var App_GUI_State = PS["App.GUI.State"];
  var App_Model_Async = PS["App.Model.Async"];
  var Data_Lens_Setter = PS["Data.Lens.Setter"];
  var Data_Profunctor_Strong = PS["Data.Profunctor.Strong"];
  var Data_Profunctor_Choice = PS["Data.Profunctor.Choice"];
  var OpticUI_Core = PS["OpticUI.Core"];
  var Data_Lens_Getter = PS["Data.Lens.Getter"];
  var Data_Lens_Internal_Forget = PS["Data.Lens.Internal.Forget"];
  var Data_Lens_Prism_Maybe = PS["Data.Lens.Prism.Maybe"];        
  var LoadAll = (function () {
      function LoadAll() {

      };
      LoadAll.value = new LoadAll();
      return LoadAll;
  })();
  var Loaded = (function () {
      function Loaded(value0) {
          this.value0 = value0;
      };
      Loaded.create = function (value0) {
          return new Loaded(value0);
      };
      return Loaded;
  })();
  var LoadingFailed = (function () {
      function LoadingFailed(value0) {
          this.value0 = value0;
      };
      LoadingFailed.create = function (value0) {
          return new LoadingFailed(value0);
      };
      return LoadingFailed;
  })();
  var SaveNew = (function () {
      function SaveNew() {

      };
      SaveNew.value = new SaveNew();
      return SaveNew;
  })();
  var NewSaved = (function () {
      function NewSaved(value0) {
          this.value0 = value0;
      };
      NewSaved.create = function (value0) {
          return new NewSaved(value0);
      };
      return NewSaved;
  })();
  var NewSaveFailed = (function () {
      function NewSaveFailed(value0) {
          this.value0 = value0;
      };
      NewSaveFailed.create = function (value0) {
          return new NewSaveFailed(value0);
      };
      return NewSaveFailed;
  })();
  var StartEdit = (function () {
      function StartEdit(value0) {
          this.value0 = value0;
      };
      StartEdit.create = function (value0) {
          return new StartEdit(value0);
      };
      return StartEdit;
  })();
  var CancelEdit = (function () {
      function CancelEdit() {

      };
      CancelEdit.value = new CancelEdit();
      return CancelEdit;
  })();
  var SaveEdit = (function () {
      function SaveEdit() {

      };
      SaveEdit.value = new SaveEdit();
      return SaveEdit;
  })();
  var EditSaved = (function () {
      function EditSaved(value0) {
          this.value0 = value0;
      };
      EditSaved.create = function (value0) {
          return new EditSaved(value0);
      };
      return EditSaved;
  })();
  var EditSaveFailed = (function () {
      function EditSaveFailed(value0) {
          this.value0 = value0;
      };
      EditSaveFailed.create = function (value0) {
          return new EditSaveFailed(value0);
      };
      return EditSaveFailed;
  })();
  var StartDelete = (function () {
      function StartDelete(value0) {
          this.value0 = value0;
      };
      StartDelete.create = function (value0) {
          return new StartDelete(value0);
      };
      return StartDelete;
  })();
  var CancelDelete = (function () {
      function CancelDelete() {

      };
      CancelDelete.value = new CancelDelete();
      return CancelDelete;
  })();
  var Delete = (function () {
      function Delete(value0) {
          this.value0 = value0;
      };
      Delete.create = function (value0) {
          return new Delete(value0);
      };
      return Delete;
  })();
  var DeleteDone = (function () {
      function DeleteDone() {

      };
      DeleteDone.value = new DeleteDone();
      return DeleteDone;
  })();
  var DeleteFailed = (function () {
      function DeleteFailed(value0) {
          this.value0 = value0;
      };
      DeleteFailed.create = function (value0) {
          return new DeleteFailed(value0);
      };
      return DeleteFailed;
  })();
  var crudHandler = function (s) {
      return function (h) {
          return function (impls) {
              return function (comm) {
                  var updateEditingAndStop = function (replacement) {
                      return Data_Maybe.maybe(Prelude["return"](Control_Monad_Eff.applicativeEff)(Prelude.unit))(function (v) {
                          var updates = function ($33) {
                              return Data_Lens_Setter.set(App_GUI_State._editing(Data_Profunctor_Strong.strongFn))(Data_Maybe.Nothing.value)(Data_Lens_Setter.over(function ($34) {
                                  return App_GUI_State._collection(Data_Profunctor_Strong.strongFn)(App_Model_Async._Done(Data_Profunctor_Choice.choiceFn)($34));
                              })(function (as) {
                                  return Data_Maybe.maybe(as)(Prelude.id(Prelude.categoryFn))(Data_Array.updateAt(v.index)(replacement)(as));
                              })($33));
                          };
                          return OpticUI_Core.runHandler(h)(updates(s));
                      })(Data_Lens_Getter.view(App_GUI_State._editing(Data_Lens_Internal_Forget.strongForget))(s));
                  };
                  var handle = function (v) {
                      if (v instanceof LoadAll) {
                          return function __do() {
                              var a = OpticUI_Components_Async.async(impls.loadAll)();
                              return OpticUI_Core.runHandler(h)(Data_Lens_Setter.set(App_GUI_State._collection(Data_Profunctor_Strong.strongFn))(new App_Model_Async.Busy(a))(s))();
                          };
                      };
                      if (v instanceof SaveNew) {
                          return function __do() {
                              var a = OpticUI_Components_Async.async(impls.saveNew(impls.constr(Data_Lens_Getter.view(function ($35) {
                                  return App_GUI_State._new(Data_Lens_Internal_Forget.strongForget)(App_GUI_State._model(Data_Lens_Internal_Forget.strongForget)($35));
                              })(s))))();
                              return OpticUI_Core.runHandler(h)(Data_Lens_Setter.set(function ($36) {
                                  return App_GUI_State._new(Data_Profunctor_Strong.strongFn)(App_GUI_State._state(Data_Profunctor_Strong.strongFn)($36));
                              })(new App_Model_Async.Busy(a))(s))();
                          };
                      };
                      if (v instanceof Loaded) {
                          return OpticUI_Core.runHandler(h)(Data_Lens_Setter.set(App_GUI_State._collection(Data_Profunctor_Strong.strongFn))(new App_Model_Async.Done(v.value0))(s));
                      };
                      if (v instanceof LoadingFailed) {
                          return OpticUI_Core.runHandler(h)(Data_Lens_Setter.set(App_GUI_State._collection(Data_Profunctor_Strong.strongFn))(new App_Model_Async.Errored(v.value0))(s));
                      };
                      if (v instanceof Delete) {
                          return function __do() {
                              var a1 = OpticUI_Components_Async.async(impls["delete"](v.value0))();
                              return OpticUI_Core.runHandler(h)(Data_Lens_Setter.set(function ($37) {
                                  return App_GUI_State._deleting(Data_Profunctor_Strong.strongFn)(Data_Lens_Prism_Maybe._Just(Data_Profunctor_Choice.choiceFn)(App_GUI_State._saving(Data_Profunctor_Strong.strongFn)($37)));
                              })(new App_Model_Async.Busy(a1))(s))();
                          };
                      };
                      if (v instanceof StartDelete) {
                          return OpticUI_Core.runHandler(h)(Data_Lens_Setter.set(App_GUI_State._deleting(Data_Profunctor_Strong.strongFn))(new Data_Maybe.Just({
                              index: v.value0, 
                              saving: App_Model_Async.Initial.value
                          }))(s));
                      };
                      if (v instanceof CancelDelete) {
                          return OpticUI_Core.runHandler(h)(Data_Lens_Setter.set(App_GUI_State._deleting(Data_Profunctor_Strong.strongFn))(Data_Maybe.Nothing.value)(s));
                      };
                      if (v instanceof DeleteDone) {
                          var index = Prelude[">>="](Data_Maybe.bindMaybe)(Data_Lens_Getter.view(App_GUI_State._deleting(Data_Lens_Internal_Forget.strongForget))(s))(function (v1) {
                              return Prelude["return"](Data_Maybe.applicativeMaybe)(v1.index);
                          });
                          var coll = Data_Lens_Getter.view(function ($38) {
                              return App_GUI_State._collection(Data_Lens_Internal_Forget.strongForget)(App_Model_Async._Done(Data_Lens_Internal_Forget.choiceForget(Data_Monoid.monoidArray))($38));
                          })(s);
                          var updates = function ($39) {
                              return Data_Lens_Setter.set(App_GUI_State._deleting(Data_Profunctor_Strong.strongFn))(Data_Maybe.Nothing.value)((function () {
                                  var $16 = {
                                      i: index, 
                                      c: coll
                                  };
                                  if ($16.i instanceof Data_Maybe.Nothing) {
                                      return Prelude.id(Prelude.categoryFn);
                                  };
                                  if ($16.c.length === 0) {
                                      return Prelude.id(Prelude.categoryFn);
                                  };
                                  if ($16.i instanceof Data_Maybe.Just) {
                                      return Data_Lens_Setter.over(function ($40) {
                                          return App_GUI_State._collection(Data_Profunctor_Strong.strongFn)(App_Model_Async._Done(Data_Profunctor_Choice.choiceFn)($40));
                                      })(function (arr) {
                                          return Data_Maybe.maybe(coll)(Prelude.id(Prelude.categoryFn))(Data_Array.deleteAt($16.i.value0)(arr));
                                      });
                                  };
                                  throw new Error("Failed pattern match at App.GUI.Views.Crud line 77, column 20 - line 80, column 112: " + [ $16.constructor.name ]);
                              })()($39));
                          };
                          return OpticUI_Core.runHandler(h)(updates(s));
                      };
                      if (v instanceof DeleteFailed) {
                          return OpticUI_Core.runHandler(h)(Data_Lens_Setter.set(function ($41) {
                              return App_GUI_State._deleting(Data_Profunctor_Strong.strongFn)(Data_Lens_Prism_Maybe._Just(Data_Profunctor_Choice.choiceFn)(App_GUI_State._saving(Data_Profunctor_Strong.strongFn)($41)));
                          })(new App_Model_Async.Errored(v.value0))(s));
                      };
                      if (v instanceof NewSaveFailed) {
                          return OpticUI_Core.runHandler(h)(Data_Lens_Setter.set(function ($42) {
                              return App_GUI_State._new(Data_Profunctor_Strong.strongFn)(App_GUI_State._state(Data_Profunctor_Strong.strongFn)($42));
                          })(new App_Model_Async.Errored(v.value0))(s));
                      };
                      if (v instanceof NewSaved) {
                          var updates = function (init) {
                              return function ($43) {
                                  return Data_Lens_Setter.over(function ($44) {
                                      return App_GUI_State._collection(Data_Profunctor_Strong.strongFn)(App_Model_Async._Done(Data_Profunctor_Choice.choiceFn)($44));
                                  })(function (arr) {
                                      return Data_Array.cons(v.value0)(arr);
                                  })(Data_Lens_Setter.set(function ($45) {
                                      return App_GUI_State._new(Data_Profunctor_Strong.strongFn)(App_GUI_State._model(Data_Profunctor_Strong.strongFn)($45));
                                  })(init)(Data_Lens_Setter.set(function ($46) {
                                      return App_GUI_State._new(Data_Profunctor_Strong.strongFn)(App_GUI_State._state(Data_Profunctor_Strong.strongFn)($46));
                                  })(App_Model_Async.Initial.value)($43)));
                              };
                          };
                          return function __do() {
                              var v1 = impls.initial();
                              return OpticUI_Core.runHandler(h)(updates(v1)(s))();
                          };
                      };
                      if (v instanceof StartEdit) {
                          return Data_Maybe.maybe(Prelude["return"](Control_Monad_Eff.applicativeEff)(Prelude.unit))(function (a) {
                              return OpticUI_Core.runHandler(h)(Data_Lens_Setter.set(App_GUI_State._editing(Data_Profunctor_Strong.strongFn))(new Data_Maybe.Just({
                                  index: v.value0, 
                                  previous: a, 
                                  saving: App_Model_Async.Initial.value
                              }))(s));
                          })(Data_Array["!!"](Data_Lens_Getter.view(function ($47) {
                              return App_GUI_State._collection(Data_Lens_Internal_Forget.strongForget)(App_Model_Async._Done(Data_Lens_Internal_Forget.choiceForget(Data_Monoid.monoidArray))($47));
                          })(s))(v.value0));
                      };
                      if (v instanceof CancelEdit) {
                          return Data_Maybe.maybe(Prelude["return"](Control_Monad_Eff.applicativeEff)(Prelude.unit))(function (v1) {
                              return updateEditingAndStop(v1.previous);
                          })(Data_Lens_Getter.view(App_GUI_State._editing(Data_Lens_Internal_Forget.strongForget))(s));
                      };
                      if (v instanceof SaveEdit) {
                          return Data_Maybe.maybe(Prelude["return"](Control_Monad_Eff.applicativeEff)(Prelude.unit))(function (a) {
                              return function __do() {
                                  var b = OpticUI_Components_Async.async(impls.saveEdit(a))();
                                  return OpticUI_Core.runHandler(h)(Data_Lens_Setter.set(function ($48) {
                                      return App_GUI_State._editing(Data_Profunctor_Strong.strongFn)(Data_Lens_Prism_Maybe._Just(Data_Profunctor_Choice.choiceFn)(App_GUI_State._saving(Data_Profunctor_Strong.strongFn)($48)));
                                  })(new App_Model_Async.Busy(b))(s))();
                              };
                          })(Prelude[">>="](Data_Maybe.bindMaybe)(Data_Lens_Getter.view(App_GUI_State._editing(Data_Lens_Internal_Forget.strongForget))(s))(function (v1) {
                              return Data_Array["!!"](Data_Lens_Getter.view(function ($49) {
                                  return App_GUI_State._collection(Data_Lens_Internal_Forget.strongForget)(App_Model_Async._Done(Data_Lens_Internal_Forget.choiceForget(Data_Monoid.monoidArray))($49));
                              })(s))(v1.index);
                          }));
                      };
                      if (v instanceof EditSaved) {
                          return updateEditingAndStop(v.value0);
                      };
                      if (v instanceof EditSaveFailed) {
                          return OpticUI_Core.runHandler(h)(Data_Lens_Setter.set(function ($50) {
                              return App_GUI_State._editing(Data_Profunctor_Strong.strongFn)(Data_Lens_Prism_Maybe._Just(Data_Profunctor_Choice.choiceFn)(App_GUI_State._saving(Data_Profunctor_Strong.strongFn)($50)));
                          })(new App_Model_Async.Errored(v.value0))(s));
                      };
                      throw new Error("Failed pattern match at App.GUI.Views.Crud line 57, column 1 - line 100, column 101: " + [ v.constructor.name ]);
                  };
                  return handle(comm);
              };
          };
      };
  };
  exports["LoadAll"] = LoadAll;
  exports["Loaded"] = Loaded;
  exports["LoadingFailed"] = LoadingFailed;
  exports["SaveNew"] = SaveNew;
  exports["NewSaved"] = NewSaved;
  exports["NewSaveFailed"] = NewSaveFailed;
  exports["StartEdit"] = StartEdit;
  exports["CancelEdit"] = CancelEdit;
  exports["SaveEdit"] = SaveEdit;
  exports["EditSaved"] = EditSaved;
  exports["EditSaveFailed"] = EditSaveFailed;
  exports["StartDelete"] = StartDelete;
  exports["CancelDelete"] = CancelDelete;
  exports["Delete"] = Delete;
  exports["DeleteDone"] = DeleteDone;
  exports["DeleteFailed"] = DeleteFailed;
  exports["crudHandler"] = crudHandler;
})(PS["App.GUI.Views.Crud"] = PS["App.GUI.Views.Crud"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var OpticUI_Markup_HTML = PS["OpticUI.Markup.HTML"];
  var OpticUI_Markup = PS["OpticUI.Markup"];
  var OpticUI = PS["OpticUI"];
  var OpticUI_Components_Async = PS["OpticUI.Components.Async"];
  var Control_Monad_Eff_Exception = PS["Control.Monad.Eff.Exception"];
  var Control_Monad_Aff = PS["Control.Monad.Aff"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_StrMap = PS["Data.StrMap"];
  var Network_HTTP_Affjax = PS["Network.HTTP.Affjax"];
  var App_Model_Profile = PS["App.Model.Profile"];
  var App_Model_Async = PS["App.Model.Async"];
  var App_Model_StrMap = PS["App.Model.StrMap"];
  var App_GUI_Types = PS["App.GUI.Types"];
  var App_GUI_Components_Exec = PS["App.GUI.Components.Exec"];
  var App_Endpoint = PS["App.Endpoint"];
  var Endpoint_Client = PS["Endpoint.Client"];
  var Data_Serializable = PS["Data.Serializable"];
  var Data_Generic = PS["Data.Generic"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var OpticUI_Core = PS["OpticUI.Core"];        
  var allProfiles = Prelude["<$>"](Control_Monad_Aff.functorAff)(App_Model_StrMap.fromArray)(Endpoint_Client.execEndpoint(Data_Serializable.serializableUnit)(Data_Generic.genericUnit)(Data_Generic.genericArray(Data_Generic.genericTuple(Data_Generic.genericString)(Data_Generic.genericArray(Data_Generic.genericString))))(App_Endpoint.getProfiles)(Prelude.unit)(Prelude.unit));
  var loadProfiles = (function () {
      var c = function (v) {
          return function (v1) {
              if (v instanceof App_Model_Async.Initial) {
                  return App_GUI_Components_Exec.exec(OpticUI_Markup.markupMonoid)(function __do() {
                      var a = OpticUI_Components_Async.async(allProfiles)();
                      return OpticUI_Core.runHandler(v1)(new App_Model_Async.Busy(a))();
                  });
              };
              if (v instanceof App_Model_Async.Busy) {
                  return Prelude["<>"](OpticUI_Core.uiSemigroup(OpticUI_Markup.markupSemigroup))(OpticUI_Core.ui(OpticUI_Markup_HTML.div([  ])(OpticUI_Markup.text("Loading profiles"))))(App_Model_Async._Busy(OpticUI_Core.uiChoice(OpticUI_Markup.markupMonoid))(OpticUI_Components_Async.onResult(OpticUI_Markup.markupMonoid)(function ($6) {
                      return OpticUI_Core.runHandler(v1)(App_Model_Async.Done.create($6));
                  })(function ($7) {
                      return OpticUI_Core.runHandler(v1)(App_Model_Async.Errored.create($7));
                  })));
              };
              if (v instanceof App_Model_Async.Errored) {
                  return OpticUI_Core.ui(OpticUI_Markup_HTML.div([ OpticUI_Markup_HTML.classA("alert alert-danger") ])(OpticUI_Markup.text("Failed to load profiles: " + Control_Monad_Eff_Exception.message(v.value0))));
              };
              return Data_Monoid.mempty(OpticUI_Core.uiMonoid(OpticUI_Markup.markupMonoid));
          };
      };
      return OpticUI_Core["with"](c);
  })();
  exports["allProfiles"] = allProfiles;
  exports["loadProfiles"] = loadProfiles;
})(PS["App.GUI.Views.Profiles"] = PS["App.GUI.Views.Profiles"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var OpticUI_Markup_HTML = PS["OpticUI.Markup.HTML"];
  var OpticUI_Markup = PS["OpticUI.Markup"];
  var OpticUI_Core = PS["OpticUI.Core"];
  var OpticUI_Components_Async = PS["OpticUI.Components.Async"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Control_Monad_Eff_Exception = PS["Control.Monad.Eff.Exception"];
  var Control_Monad_Eff_Ref = PS["Control.Monad.Eff.Ref"];
  var Data_Maybe = PS["Data.Maybe"];
  var App_GUI_Types = PS["App.GUI.Types"];
  var App_GUI_Views_Crud = PS["App.GUI.Views.Crud"];
  var App_Model_Async = PS["App.Model.Async"];        
  var newButton = function (handle) {
      var c = function (v) {
          return function (h) {
              if (v instanceof App_Model_Async.Busy) {
                  return Prelude["<>"](OpticUI_Core.uiSemigroup(OpticUI_Markup.markupSemigroup))(OpticUI_Core.ui(OpticUI_Markup_HTML.button([ OpticUI_Markup_HTML.classA("btn btn-warning") ])(OpticUI_Markup.text("Saving..."))))(App_Model_Async._Busy(OpticUI_Core.uiChoice(OpticUI_Markup.markupMonoid))(OpticUI_Components_Async.onResult(OpticUI_Markup.markupMonoid)(function ($27) {
                      return handle(App_GUI_Views_Crud.NewSaved.create($27));
                  })(function ($28) {
                      return handle(App_GUI_Views_Crud.NewSaveFailed.create($28));
                  })));
              };
              if (v instanceof App_Model_Async.Errored) {
                  return Prelude["<>"](OpticUI_Core.uiSemigroup(OpticUI_Markup.markupSemigroup))(OpticUI_Core.ui(OpticUI_Markup_HTML.button([ OpticUI_Markup_HTML.classA("btn btn-danger"), OpticUI_Markup_HTML.onClick(function (v1) {
                      return handle(App_GUI_Views_Crud.SaveNew.value);
                  }) ])(OpticUI_Markup.text("Saving Failed, Try Again?"))))(OpticUI_Core.ui(OpticUI_Markup_HTML.div([ OpticUI_Markup_HTML.classA("alert alert-danger") ])(OpticUI_Markup.text(Control_Monad_Eff_Exception.message(v.value0)))));
              };
              return OpticUI_Core.ui(OpticUI_Markup_HTML.button([ OpticUI_Markup_HTML.classA("btn btn-action"), OpticUI_Markup_HTML.onClick(function (v1) {
                  return handle(App_GUI_Views_Crud.SaveNew.value);
              }) ])(OpticUI_Markup.text("Save!")));
          };
      };
      return OpticUI_Core["with"](c);
  };
  var editButton = function (handle) {
      return function (i) {
          return function (editing) {
              var c = function (v) {
                  if (v instanceof Data_Maybe.Nothing) {
                      return OpticUI_Core.ui(OpticUI_Markup_HTML.button([ OpticUI_Markup_HTML.classA("btn btn-action"), OpticUI_Markup_HTML.onClick(Prelude["const"](handle(new App_GUI_Views_Crud.StartEdit(i)))) ])(OpticUI_Markup.text("Edit!")));
                  };
                  if (v instanceof Data_Maybe.Just && v.value0.index !== i) {
                      return OpticUI_Core.ui(OpticUI_Markup_HTML.div([  ])(OpticUI_Markup.text("")));
                  };
                  if (v instanceof Data_Maybe.Just && v.value0.saving instanceof App_Model_Async.Busy) {
                      return OpticUI_Core.ui(OpticUI_Markup_HTML.button([ OpticUI_Markup_HTML.classA("btn btn-warning") ])(OpticUI_Markup.text("Saving")));
                  };
                  if (v instanceof Data_Maybe.Just && v.value0.saving instanceof App_Model_Async.Errored) {
                      return Prelude["<>"](OpticUI_Core.uiSemigroup(OpticUI_Markup.markupSemigroup))(OpticUI_Core.ui(OpticUI_Markup_HTML.button([ OpticUI_Markup_HTML.classA("btn btn-danger"), OpticUI_Markup_HTML.onClick(function (v1) {
                          return handle(App_GUI_Views_Crud.SaveEdit.value);
                      }) ])(OpticUI_Markup.text("Save failed, try again"))))(OpticUI_Core.ui(OpticUI_Markup_HTML.div([ OpticUI_Markup_HTML.classA("alert alert-danger") ])(OpticUI_Markup.text(Control_Monad_Eff_Exception.message(v.value0.saving.value0)))));
                  };
                  if (v instanceof Data_Maybe.Just) {
                      return Prelude["<>"](OpticUI_Core.uiSemigroup(OpticUI_Markup.markupSemigroup))(OpticUI_Core.ui(OpticUI_Markup_HTML.button([ OpticUI_Markup_HTML.classA("btn btn-success"), OpticUI_Markup_HTML.onClick(function (v1) {
                          return handle(App_GUI_Views_Crud.SaveEdit.value);
                      }) ])(OpticUI_Markup.text("Save edit"))))(OpticUI_Core.ui(OpticUI_Markup_HTML.button([ OpticUI_Markup_HTML.classA("btn btn-warning"), OpticUI_Markup_HTML.onClick(function (v1) {
                          return handle(App_GUI_Views_Crud.CancelEdit.value);
                      }) ])(OpticUI_Markup.text("Cancel edit"))));
                  };
                  throw new Error("Failed pattern match at App.GUI.Components.CrudButtons line 23, column 5 - line 24, column 5: " + [ v.constructor.name ]);
              };
              return c(editing);
          };
      };
  };
  exports["newButton"] = newButton;
  exports["editButton"] = editButton;
})(PS["App.GUI.Components.CrudButtons"] = PS["App.GUI.Components.CrudButtons"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var OpticUI_Markup_HTML = PS["OpticUI.Markup.HTML"];
  var OpticUI_Markup = PS["OpticUI.Markup"];
  var OpticUI_Components = PS["OpticUI.Components"];
  var OpticUI = PS["OpticUI"];
  var OpticUI_Components_Async = PS["OpticUI.Components.Async"];
  var Control_Monad_Aff = PS["Control.Monad.Aff"];
  var Control_Monad_Eff_Exception = PS["Control.Monad.Eff.Exception"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Date = PS["Data.Date"];
  var Data_Foldable = PS["Data.Foldable"];
  var Data_Lens_Common = PS["Data.Lens.Common"];
  var Data_Lens = PS["Data.Lens"];
  var Data_Lens_Traversal = PS["Data.Lens.Traversal"];
  var Data_Tuple = PS["Data.Tuple"];
  var Data_Array = PS["Data.Array"];
  var Data_StrMap = PS["Data.StrMap"];
  var DOM_File_Types = PS["DOM.File.Types"];
  var Network_HTTP_Affjax = PS["Network.HTTP.Affjax"];
  var App_Model_Event = PS["App.Model.Event"];
  var App_Model_Date = PS["App.Model.Date"];
  var App_Model_SavedFile = PS["App.Model.SavedFile"];
  var App_Model_Async = PS["App.Model.Async"];
  var App_Model_Profile = PS["App.Model.Profile"];
  var App_GUI_Types = PS["App.GUI.Types"];
  var App_GUI_State = PS["App.GUI.State"];
  var App_GUI_Components_Exec = PS["App.GUI.Components.Exec"];
  var App_GUI_Components_FileInput = PS["App.GUI.Components.FileInput"];
  var App_GUI_Components_Select = PS["App.GUI.Components.Select"];
  var App_GUI_Components_DateTimeField = PS["App.GUI.Components.DateTimeField"];
  var App_GUI_Components_Markup = PS["App.GUI.Components.Markup"];
  var App_GUI_Views_Crud = PS["App.GUI.Views.Crud"];
  var App_GUI_Views_Profiles = PS["App.GUI.Views.Profiles"];
  var App_Endpoint = PS["App.Endpoint"];
  var App_GUI_Load = PS["App.GUI.Load"];
  var App_GUI_Router = PS["App.GUI.Router"];
  var App_GUI_Components_CrudButtons = PS["App.GUI.Components.CrudButtons"];
  var Endpoint_Client = PS["Endpoint.Client"];
  var Data_Serializable = PS["Data.Serializable"];
  var Data_Lens_Getter = PS["Data.Lens.Getter"];
  var Data_Lens_Internal_Forget = PS["Data.Lens.Internal.Forget"];
  var OpticUI_Core = PS["OpticUI.Core"];
  var Data_Traversable = PS["Data.Traversable"];
  var Data_Lens_Setter = PS["Data.Lens.Setter"];
  var Data_Profunctor_Strong = PS["Data.Profunctor.Strong"];
  var Data_Lens_Prism_Maybe = PS["Data.Lens.Prism.Maybe"];
  var Data_Monoid = PS["Data.Monoid"];        
  var Crud = (function () {
      function Crud(value0) {
          this.value0 = value0;
      };
      Crud.create = function (value0) {
          return new Crud(value0);
      };
      return Crud;
  })();
  var saveUpdatedEvent = function (i) {
      return Prelude[">>="](Control_Monad_Aff.bindAff)(Endpoint_Client.execEndpoint(Data_Serializable.serializableUnit)(App_Model_Event.genericEvent)(App_Model_Event.genericEvent)(App_Endpoint.putEvents)(Prelude.unit)(Data_Lens_Getter.view(App_GUI_State._model(Data_Lens_Internal_Forget.strongForget))(i)))(function (n) {
          return Prelude["return"](Control_Monad_Aff.applicativeAff)((function () {
              var $13 = {};
              for (var $14 in i) {
                  if (i.hasOwnProperty($14)) {
                      $13[$14] = i[$14];
                  };
              };
              $13.model = n;
              return $13;
          })());
      });
  };
  var saveNewEvent = function (i) {
      return Prelude[">>="](Control_Monad_Aff.bindAff)(Endpoint_Client.execEndpoint(Data_Serializable.serializableUnit)(App_Model_Event.genericEvent)(App_Model_Event.genericEvent)(App_Endpoint.postEvents)(Prelude.unit)(Data_Lens_Getter.view(App_GUI_State._model(Data_Lens_Internal_Forget.strongForget))(i)))(function (n) {
          return Prelude["return"](Control_Monad_Aff.applicativeAff)((function () {
              var $15 = {};
              for (var $16 in i) {
                  if (i.hasOwnProperty($16)) {
                      $15[$16] = i[$16];
                  };
              };
              $15.model = n;
              return $15;
          })());
      });
  };
  var saveFile = function (file) {
      return function (i) {
          return Endpoint_Client.execFileUploadEndpoint(Data_Serializable.serializableTuple(Data_Serializable.serializableInt)(Data_Serializable.serializableString))(App_Model_SavedFile.genericSavedFile)(App_Endpoint.attachFile)(file)(new Data_Tuple.Tuple(i, App_GUI_Components_FileInput.name(file)));
      };
  };
  var showEvents = function (handle) {
      return function (profiles) {
          var listFiles = App_GUI_State._model(OpticUI_Core.uiStrong)(App_Model_Event._Event(OpticUI_Core.uiStrong)(App_GUI_State._files(OpticUI_Core.uiStrong)(OpticUI_Core.traversal(OpticUI_Markup.markupMonoid)(function (dictWander) {
              return Data_Lens_Traversal.traversed(Data_Traversable.traversableArray)(dictWander);
          })(OpticUI_Core["with"](function (v) {
              return function (v1) {
                  return OpticUI_Core.ui(OpticUI_Markup_HTML.div([  ])(OpticUI_Markup.text(v.value0.name)));
              };
          })))));
          var line = function (v) {
              return function (editing) {
                  return function (i) {
                      return function (v1) {
                          return function (h) {
                              if (v instanceof Data_Maybe.Just && v.value0 === i) {
                                  return Data_Foldable.mconcat(Data_Foldable.foldableArray)(OpticUI_Core.uiMonoid(OpticUI_Markup.markupMonoid))(Prelude["<$>"](Prelude.functorArray)(OpticUI_Core.withView(OpticUI_Markup_HTML.td([  ])))([ OpticUI_Core.ui(OpticUI_Markup.text(Data_Maybe.maybe("")(Prelude.show(Prelude.showInt))(v1.model.value0.id))), OpticUI_Core.ui(OpticUI_Markup.text(v1.model.value0.computername)), App_GUI_State._model(OpticUI_Core.uiStrong)(App_Model_Event._Event(OpticUI_Core.uiStrong)(App_GUI_State._name(OpticUI_Core.uiStrong)(OpticUI_Components.textField([ OpticUI_Markup_HTML.classA("form-control") ])))), App_GUI_State._model(OpticUI_Core.uiStrong)(App_Model_Event._Event(OpticUI_Core.uiStrong)(App_GUI_State._datefrom(OpticUI_Core.uiStrong)(App_GUI_Components_DateTimeField.dateTimeField([ OpticUI_Markup_HTML.classA("form-control") ])))), App_GUI_State._model(OpticUI_Core.uiStrong)(App_Model_Event._Event(OpticUI_Core.uiStrong)(App_GUI_State._dateuntil(OpticUI_Core.uiStrong)(App_GUI_Components_DateTimeField.dateTimeField([ OpticUI_Markup_HTML.classA("form-control") ])))), App_GUI_State._model(OpticUI_Core.uiStrong)(App_Model_Event._Event(OpticUI_Core.uiStrong)(App_GUI_State._profile(OpticUI_Core.uiStrong)(App_GUI_Components_Select.select(Data_Maybe.fromMaybe([  ])(Data_StrMap.lookup(v1.model.value0.computername)(profiles)))(Prelude.id(Prelude.categoryFn))([ OpticUI_Markup_HTML.classA("form-control") ])))), App_GUI_Components_CrudButtons.editButton(function ($61) {
                                      return handle(Crud.create($61));
                                  })(i)(editing), Prelude["<>"](OpticUI_Core.uiSemigroup(OpticUI_Markup.markupSemigroup))(OpticUI_Core.ui(OpticUI_Markup_HTML.br([  ])(OpticUI_Markup.text(""))))(listFiles) ]));
                              };
                              var fileSelected = function (v2) {
                                  return function (v3) {
                                      if (v3 instanceof Data_Maybe.Nothing) {
                                          return Prelude["return"](Control_Monad_Eff.applicativeEff)(Prelude.unit);
                                      };
                                      if (v3 instanceof Data_Maybe.Just) {
                                          return function __do() {
                                              var a = OpticUI_Components_Async.async(saveFile(v3.value0)(Data_Maybe.maybe(-1)(Prelude.id(Prelude.categoryFn))(v1.model.value0.id)))();
                                              return OpticUI_Core.runHandler(h)((function () {
                                                  var $32 = {};
                                                  for (var $33 in v1) {
                                                      if (v1.hasOwnProperty($33)) {
                                                          $32[$33] = v1[$33];
                                                      };
                                                  };
                                                  $32.state = (function () {
                                                      var $30 = {};
                                                      for (var $31 in v1.state) {
                                                          if (v1.state.hasOwnProperty($31)) {
                                                              $30[$31] = v1.state[$31];
                                                          };
                                                      };
                                                      $30.savingFile = new App_Model_Async.Busy(a);
                                                      return $30;
                                                  })();
                                                  return $32;
                                              })())();
                                          };
                                      };
                                      throw new Error("Failed pattern match at App.GUI.Views.EventsPage line 116, column 11 - line 117, column 8: " + [ v2.constructor.name, v3.constructor.name ]);
                                  };
                              };
                              var fileSaved = function (si) {
                                  return OpticUI_Core.runHandler(h)({
                                      model: Data_Lens_Setter.over(function ($62) {
                                          return App_Model_Event._Event(Data_Profunctor_Strong.strongFn)(App_GUI_State._files(Data_Profunctor_Strong.strongFn)($62));
                                      })(Data_Array.cons(si))(v1.model), 
                                      state: (function () {
                                          var $35 = {};
                                          for (var $36 in v1.state) {
                                              if (v1.state.hasOwnProperty($36)) {
                                                  $35[$36] = v1.state[$36];
                                              };
                                          };
                                          $35.savingFile = App_Model_Async.Initial.value;
                                          $35.file = Data_Maybe.Nothing.value;
                                          return $35;
                                      })()
                                  });
                              };
                              var fileSaveErrored = function (err) {
                                  return OpticUI_Core.runHandler(h)((function () {
                                      var $39 = {};
                                      for (var $40 in v1) {
                                          if (v1.hasOwnProperty($40)) {
                                              $39[$40] = v1[$40];
                                          };
                                      };
                                      $39.state = (function () {
                                          var $37 = {};
                                          for (var $38 in v1.state) {
                                              if (v1.state.hasOwnProperty($38)) {
                                                  $37[$38] = v1.state[$38];
                                              };
                                          };
                                          $37.savingFile = new App_Model_Async.Errored(err);
                                          return $37;
                                      })();
                                      return $39;
                                  })());
                              };
                              var fileIdStr = Data_Maybe.maybe("")(Prelude.show(Prelude.showInt))(v1.model.value0.id);
                              var fileInputId = "event-fileinput-" + fileIdStr;
                              return Data_Foldable.mconcat(Data_Foldable.foldableArray)(OpticUI_Core.uiMonoid(OpticUI_Markup.markupMonoid))([ OpticUI_Core.ui(OpticUI_Markup_HTML.td([  ])(OpticUI_Markup.text(fileIdStr))), OpticUI_Core.ui(OpticUI_Markup_HTML.td([  ])(OpticUI_Markup.text(v1.model.value0.computername))), OpticUI_Core.ui(OpticUI_Markup_HTML.td([  ])(OpticUI_Markup.text(v1.model.value0.name))), OpticUI_Core.ui(OpticUI_Markup_HTML.td([  ])(OpticUI_Markup.text(App_Model_Date.toLocalDatetime(v1.model.value0.datefrom)))), OpticUI_Core.ui(OpticUI_Markup_HTML.td([  ])(OpticUI_Markup.text(App_Model_Date.toLocalDatetime(v1.model.value0.dateuntil)))), OpticUI_Core.ui(OpticUI_Markup_HTML.td([  ])(OpticUI_Markup.text(v1.model.value0.profile))), OpticUI_Core.withView(OpticUI_Markup_HTML.td([  ]))(App_GUI_Components_CrudButtons.editButton(function ($63) {
                                  return handle(Crud.create($63));
                              })(i)(editing)), OpticUI_Core.withView(OpticUI_Markup_HTML.td([  ]))(Data_Foldable.mconcat(Data_Foldable.foldableArray)(OpticUI_Core.uiMonoid(OpticUI_Markup.markupMonoid))([ App_GUI_State._state(OpticUI_Core.uiStrong)(App_GUI_State._file(OpticUI_Core.uiStrong)(Data_Foldable.mconcat(Data_Foldable.foldableArray)(OpticUI_Core.uiMonoid(OpticUI_Markup.markupMonoid))([ OpticUI_Core.ui(OpticUI_Markup_HTML.label([ OpticUI_Markup.attr("for")(fileInputId), OpticUI_Markup_HTML.classA("btn-action btn") ])(OpticUI_Markup.text("Upload"))), App_GUI_Components_FileInput.fileInput([ App_GUI_Components_FileInput.onFileInput(fileSelected), OpticUI_Markup.attr("id")(fileInputId) ]) ]))), listFiles, App_GUI_State._state(OpticUI_Core.uiStrong)(App_GUI_State._savingFile(OpticUI_Core.uiStrong)(App_Model_Async._Errored(OpticUI_Core.uiChoice(OpticUI_Markup.markupMonoid))(OpticUI_Core["with"](function (err) {
                                  return function (v2) {
                                      return OpticUI_Core.ui(OpticUI_Markup_HTML.div([ OpticUI_Markup_HTML.classA("alert alert-danger") ])(OpticUI_Markup.text(Control_Monad_Eff_Exception.message(err))));
                                  };
                              })))) ])), App_GUI_State._state(OpticUI_Core.uiStrong)(App_GUI_State._savingFile(OpticUI_Core.uiStrong)(App_Model_Async._Busy(OpticUI_Core.uiChoice(OpticUI_Markup.markupMonoid))(OpticUI_Components_Async.onResult(OpticUI_Markup.markupMonoid)(fileSaved)(fileSaveErrored)))) ]);
                          };
                      };
                  };
              };
          };
          var c = function (v) {
              return function (h) {
                  if (v.collection instanceof App_Model_Async.Initial) {
                      return OpticUI_Core.ui(OpticUI_Markup_HTML.tr([  ])(OpticUI_Markup_HTML.td([  ])(OpticUI_Markup.text("No events loaded yet, loading..."))));
                  };
                  if (v.collection instanceof App_Model_Async.Busy) {
                      return OpticUI_Core.ui(OpticUI_Markup_HTML.tr([  ])(OpticUI_Markup_HTML.td([  ])(OpticUI_Markup.text("Loading events"))));
                  };
                  if (v.collection instanceof App_Model_Async.Errored) {
                      return OpticUI_Core.ui(OpticUI_Markup_HTML.tr([  ])(OpticUI_Markup_HTML.td([  ])(OpticUI_Markup_HTML.div([ OpticUI_Markup_HTML.classA("alert alert-danger") ])(OpticUI_Markup.text("Events failed to load: " + Control_Monad_Eff_Exception.message(v.collection.value0))))));
                  };
                  if (v.collection instanceof App_Model_Async.Done) {
                      var selI = Data_Maybe.maybe(Data_Maybe.Nothing.value)(function (ed) {
                          return new Data_Maybe.Just(ed.index);
                      })(v.editing);
                      var editing = Data_Lens_Getter.view(App_GUI_State._editing(Data_Lens_Internal_Forget.strongForget))(v);
                      return App_GUI_State._collection(OpticUI_Core.uiStrong)(App_Model_Async._Done(OpticUI_Core.uiChoice(OpticUI_Markup.markupMonoid))(OpticUI_Core.foreach(OpticUI_Markup.markupMonoid)(Data_Traversable.traversableArray)(function (i) {
                          return OpticUI_Core.withView(OpticUI_Markup_HTML.tr([  ]))(OpticUI_Core["with"](line(selI)(editing)(i)));
                      })));
                  };
                  throw new Error("Failed pattern match at App.GUI.Views.EventsPage line 88, column 1 - line 135, column 1: " + [ v.constructor.name, h.constructor.name ]);
              };
          };
          return Prelude["<>"](OpticUI_Core.uiSemigroup(OpticUI_Markup.markupSemigroup))(OpticUI_Core["with"](c))(Prelude["<>"](OpticUI_Core.uiSemigroup(OpticUI_Markup.markupSemigroup))(App_GUI_State._collection(OpticUI_Core.uiStrong)(App_Model_Async._Initial(OpticUI_Core.uiChoice(OpticUI_Markup.markupMonoid))(App_GUI_Components_Exec.exec(OpticUI_Markup.markupMonoid)(handle(new Crud(App_GUI_Views_Crud.LoadAll.value))))))(Prelude["<>"](OpticUI_Core.uiSemigroup(OpticUI_Markup.markupSemigroup))(App_GUI_State._collection(OpticUI_Core.uiStrong)(App_Model_Async._Busy(OpticUI_Core.uiChoice(OpticUI_Markup.markupMonoid))(OpticUI_Components_Async.onResult(OpticUI_Markup.markupMonoid)(function ($64) {
              return handle(Crud.create(App_GUI_Views_Crud.Loaded.create($64)));
          })(function ($65) {
              return handle(Crud.create(App_GUI_Views_Crud.LoadingFailed.create($65)));
          }))))(App_GUI_State._editing(OpticUI_Core.uiStrong)(Data_Lens_Prism_Maybe._Just(OpticUI_Core.uiChoice(OpticUI_Markup.markupMonoid))(App_GUI_State._saving(OpticUI_Core.uiStrong)(App_Model_Async._Busy(OpticUI_Core.uiChoice(OpticUI_Markup.markupMonoid))(OpticUI_Components_Async.onResult(OpticUI_Markup.markupMonoid)(function ($66) {
              return handle(Crud.create(App_GUI_Views_Crud.EditSaved.create($66)));
          })(function ($67) {
              return handle(Crud.create(App_GUI_Views_Crud.EditSaveFailed.create($67)));
          }))))))));
      };
  };
  var makeNewEvent = function (handle) {
      return OpticUI_Core["with"](function (s) {
          return function (h) {
              return App_GUI_Components_Markup.rowUI([ OpticUI_Core.ui(OpticUI_Markup.text("")), OpticUI_Core.ui(OpticUI_Markup.text(s.model.computername)), App_GUI_State._model(OpticUI_Core.uiStrong)(App_GUI_State._name(OpticUI_Core.uiStrong)(OpticUI_Components.textField([ OpticUI_Markup_HTML.classA("form-control") ]))), App_GUI_State._model(OpticUI_Core.uiStrong)(App_GUI_State._datefrom(OpticUI_Core.uiStrong)(App_GUI_Components_DateTimeField.dateTimeField([ OpticUI_Markup_HTML.classA("form-control") ]))), App_GUI_State._model(OpticUI_Core.uiStrong)(App_GUI_State._dateuntil(OpticUI_Core.uiStrong)(App_GUI_Components_DateTimeField.dateTimeField([ OpticUI_Markup_HTML.classA("form-control") ]))), OpticUI_Core.ui(OpticUI_Markup.text("")), App_GUI_State._state(OpticUI_Core.uiStrong)(App_GUI_Components_CrudButtons.newButton(function ($68) {
                  return handle(Crud.create($68));
              })), OpticUI_Core.ui(OpticUI_Markup.text("")) ]);
          };
      });
  };
  var eventsPage = function (cn) {
      return function (alias) {
          return function (page) {
              return function (nav) {
                  var c = function (s) {
                      return function (h) {
                          var impls = {
                              loadAll: App_GUI_Load.loadEventsWithState(cn)(page), 
                              saveNew: saveNewEvent, 
                              saveEdit: saveUpdatedEvent, 
                              "delete": Prelude["const"](Prelude["return"](Control_Monad_Aff.applicativeAff)(Prelude.unit)), 
                              initial: function __do() {
                                  var d = Data_Date.now();
                                  return {
                                      id: Data_Maybe.Nothing.value, 
                                      computername: cn, 
                                      name: "", 
                                      datefrom: d, 
                                      dateuntil: d, 
                                      profile: "", 
                                      files: [  ]
                                  };
                              }, 
                              constr: function (a) {
                                  return {
                                      model: new App_Model_Event.Event(a), 
                                      state: {
                                          savingFile: App_Model_Async.Initial.value, 
                                          file: Data_Maybe.Nothing.value
                                      }
                                  };
                              }
                          };
                          var handle = function (v) {
                              return App_GUI_Views_Crud.crudHandler(s)(h)(impls)(v.value0);
                          };
                          return Data_Foldable.mconcat(Data_Foldable.foldableArray)(OpticUI_Core.uiMonoid(OpticUI_Markup.markupMonoid))([ OpticUI_Core.ui(App_GUI_Components_Markup.pageTitle(Data_Foldable.mconcat(Data_Foldable.foldableArray)(OpticUI_Markup.markupMonoid)([ OpticUI_Markup_HTML.button([ OpticUI_Markup_HTML.classA("btn-nav"), OpticUI_Markup_HTML.onClick(function (v) {
                              return nav(App_GUI_State.PhotoboothsPage.value);
                          }) ])(OpticUI_Markup.text("^")), OpticUI_Markup.text(" Events for: "), OpticUI_Markup_HTML.em([  ])(OpticUI_Markup.text(alias)), OpticUI_Markup.text(", page "), OpticUI_Markup_HTML.em([  ])(OpticUI_Markup.text(Prelude.show(Prelude.showInt)(page + 1 | 0))), OpticUI_Markup_HTML.button([ OpticUI_Markup_HTML.classA("btn-nav" + (function () {
                              var $57 = page > 0;
                              if ($57) {
                                  return "";
                              };
                              if (!$57) {
                                  return " hide";
                              };
                              throw new Error("Failed pattern match at App.GUI.Views.EventsPage line 70, column 72 - line 70, column 119: " + [ $57.constructor.name ]);
                          })()), OpticUI_Markup_HTML.onClick(function (v) {
                              return nav(new App_GUI_State.EventsPage(cn, alias, page - 1));
                          }) ])(OpticUI_Markup.text("<")), OpticUI_Markup_HTML.button([ OpticUI_Markup_HTML.classA("btn-nav" + (function () {
                              var $59 = Data_Array.length(Data_Lens_Getter.view(function ($69) {
                                  return App_GUI_State._collection(Data_Lens_Internal_Forget.strongForget)(App_Model_Async._Done(Data_Lens_Internal_Forget.choiceForget(Data_Monoid.monoidArray))($69));
                              })(s)) === 20;
                              if ($59) {
                                  return "";
                              };
                              if (!$59) {
                                  return " hide";
                              };
                              throw new Error("Failed pattern match at App.GUI.Views.EventsPage line 72, column 72 - line 72, column 162: " + [ $59.constructor.name ]);
                          })()), OpticUI_Markup_HTML.onClick(function (v) {
                              return nav(new App_GUI_State.EventsPage(cn, alias, page + 1 | 0));
                          }) ])(OpticUI_Markup.text(">")) ]))), OpticUI_Core.withView(App_GUI_Components_Markup.crudTable)(Data_Foldable.mconcat(Data_Foldable.foldableArray)(OpticUI_Core.uiMonoid(OpticUI_Markup.markupMonoid))([ OpticUI_Core.ui(App_GUI_Components_Markup.tableHeader(Prelude.functorArray)(Data_Foldable.foldableArray)([ OpticUI_Markup_HTML.classA("indexed-tr") ])([ "", "Computer", "Name", "Start", "End", "Profile", "Actions", "Files" ])), App_GUI_State._new(OpticUI_Core.uiStrong)(makeNewEvent(handle)), App_GUI_State._collectionEditing(OpticUI_Core.uiStrong)(showEvents(handle)(Data_Lens_Getter.view(function ($70) {
                              return App_GUI_State._profiles(Data_Lens_Internal_Forget.strongForget)(App_Model_Async._Done(Data_Lens_Internal_Forget.choiceForget(Data_StrMap.monoidStrMap(Prelude.semigroupArray)))($70));
                          })(s))) ])), App_GUI_State._profiles(OpticUI_Core.uiStrong)(App_GUI_Views_Profiles.loadProfiles) ]);
                      };
                  };
                  return OpticUI_Core["with"](c);
              };
          };
      };
  };
  exports["eventsPage"] = eventsPage;
})(PS["App.GUI.Views.EventsPage"] = PS["App.GUI.Views.EventsPage"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var App_Endpoint = PS["App.Endpoint"];
  var App_GUI_Components_Exec = PS["App.GUI.Components.Exec"];
  var App_GUI_Router = PS["App.GUI.Router"];
  var App_GUI_State = PS["App.GUI.State"];
  var App_GUI_Types = PS["App.GUI.Types"];
  var App_Model_Async = PS["App.Model.Async"];
  var App_Model_Session = PS["App.Model.Session"];
  var Control_Monad_Eff_Exception = PS["Control.Monad.Eff.Exception"];
  var Control_Monad_Eff_Ref = PS["Control.Monad.Eff.Ref"];
  var DOM = PS["DOM"];
  var DOM_Timer = PS["DOM.Timer"];
  var Data_Foldable = PS["Data.Foldable"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Tuple = PS["Data.Tuple"];
  var Endpoint_Client = PS["Endpoint.Client"];
  var Network_HTTP_Affjax = PS["Network.HTTP.Affjax"];
  var OpticUI = PS["OpticUI"];
  var OpticUI_Components_Async = PS["OpticUI.Components.Async"];
  var OpticUI_Markup_HTML = PS["OpticUI.Markup.HTML"];
  var Prelude = PS["Prelude"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Data_Serializable = PS["Data.Serializable"];
  var Data_Generic = PS["Data.Generic"];
  var OpticUI_Core = PS["OpticUI.Core"];
  var OpticUI_Markup = PS["OpticUI.Markup"];
  var OpticUI_Components = PS["OpticUI.Components"];        
  var loginPage = function (nav) {
      var c = function (s) {
          return function (h) {
              var asyncLogin = function __do() {
                  var a = OpticUI_Components_Async.async(Endpoint_Client.execEndpoint(Data_Serializable.serializableUnit)(Data_Generic.genericTuple(Data_Generic.genericString)(Data_Generic.genericString))(App_Model_Session.genericSession)(App_Endpoint.login)(Prelude.unit)(new Data_Tuple.Tuple(s.username, s.password)))();
                  return OpticUI_Core.runHandler(h)((function () {
                      var $2 = {};
                      for (var $3 in s) {
                          if (s.hasOwnProperty($3)) {
                              $2[$3] = s[$3];
                          };
                      };
                      $2.loggingIn = new App_Model_Async.Busy(a);
                      return $2;
                  })())();
              };
              return OpticUI_Core.withView(function ($10) {
                  return OpticUI_Markup_HTML.div([ OpticUI_Markup_HTML.classA("login-page") ])(OpticUI_Markup_HTML.div([ OpticUI_Markup_HTML.classA("login-container") ])($10));
              })(Data_Foldable.mconcat(Data_Foldable.foldableArray)(OpticUI_Core.uiMonoid(OpticUI_Markup.markupMonoid))([ OpticUI_Core.withView(OpticUI_Markup_HTML.label([  ]))(Prelude["<>"](OpticUI_Core.uiSemigroup(OpticUI_Markup.markupSemigroup))(OpticUI_Core.ui(OpticUI_Markup.text("username")))(App_GUI_State._username(OpticUI_Core.uiStrong)(OpticUI_Components.textField([ OpticUI_Markup_HTML.classA("form-control") ])))), OpticUI_Core.withView(OpticUI_Markup_HTML.label([  ]))(Prelude["<>"](OpticUI_Core.uiSemigroup(OpticUI_Markup.markupSemigroup))(OpticUI_Core.ui(OpticUI_Markup.text("password")))(App_GUI_State._password(OpticUI_Core.uiStrong)(OpticUI_Components.textField([ OpticUI_Markup_HTML.classA("form-control"), OpticUI_Markup_HTML.typeA("password") ])))), OpticUI_Core.ui(OpticUI_Markup_HTML.button([ OpticUI_Markup_HTML.classA("btn btn-primary"), OpticUI_Markup_HTML.onClick(function (v) {
                  return asyncLogin;
              }) ])(OpticUI_Markup.text("Login"))), App_GUI_State._loggingIn(OpticUI_Core.uiStrong)(App_Model_Async._Errored(OpticUI_Core.uiChoice(OpticUI_Markup.markupMonoid))(OpticUI_Core["with"](function (err) {
                  return function (v) {
                      return OpticUI_Core.ui(OpticUI_Markup_HTML.div([ OpticUI_Markup_HTML.classA("alert alert-danger") ])(OpticUI_Markup.text(Control_Monad_Eff_Exception.message(err))));
                  };
              }))), App_GUI_State._loggingIn(OpticUI_Core.uiStrong)(App_Model_Async._Busy(OpticUI_Core.uiChoice(OpticUI_Markup.markupMonoid))(OpticUI_Components_Async.onResult(OpticUI_Markup.markupMonoid)(function (sess) {
                  return OpticUI_Core.runHandler(h)((function () {
                      var $6 = {};
                      for (var $7 in s) {
                          if (s.hasOwnProperty($7)) {
                              $6[$7] = s[$7];
                          };
                      };
                      $6.loggingIn = new App_Model_Async.Done(sess);
                      $6.session = new Data_Maybe.Just(sess);
                      return $6;
                  })());
              })(function (err) {
                  return OpticUI_Core.runHandler(h)((function () {
                      var $8 = {};
                      for (var $9 in s) {
                          if (s.hasOwnProperty($9)) {
                              $8[$9] = s[$9];
                          };
                      };
                      $8.loggingIn = new App_Model_Async.Errored(err);
                      return $8;
                  })());
              }))), App_GUI_State._loggingIn(OpticUI_Core.uiStrong)(App_Model_Async._Done(OpticUI_Core.uiChoice(OpticUI_Markup.markupMonoid))(App_GUI_Components_Exec.exec(OpticUI_Markup.markupMonoid)(nav(App_GUI_State.PhotoboothsPage.value)))) ]));
          };
      };
      return OpticUI_Core["with"](c);
  };
  exports["loginPage"] = loginPage;
})(PS["App.GUI.Views.LoginPage"] = PS["App.GUI.Views.LoginPage"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var OpticUI_Markup_HTML = PS["OpticUI.Markup.HTML"];
  var OpticUI_Markup = PS["OpticUI.Markup"];
  var OpticUI_Components = PS["OpticUI.Components"];
  var OpticUI = PS["OpticUI"];
  var OpticUI_Components_Async = PS["OpticUI.Components.Async"];
  var Control_Monad_Aff = PS["Control.Monad.Aff"];
  var Control_Monad_Eff_Exception = PS["Control.Monad.Eff.Exception"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Data_Lens = PS["Data.Lens"];
  var Data_Lens_Common = PS["Data.Lens.Common"];
  var Data_Foldable = PS["Data.Foldable"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_StrMap = PS["Data.StrMap"];
  var Prelude = PS["Prelude"];
  var DOM = PS["DOM"];
  var Network_HTTP_Affjax = PS["Network.HTTP.Affjax"];
  var App_Model_Photobooth = PS["App.Model.Photobooth"];
  var App_Model_Async = PS["App.Model.Async"];
  var App_Model_Profile = PS["App.Model.Profile"];
  var App_GUI_Router = PS["App.GUI.Router"];
  var App_GUI_Types = PS["App.GUI.Types"];
  var App_GUI_State = PS["App.GUI.State"];
  var App_GUI_Components_Exec = PS["App.GUI.Components.Exec"];
  var App_GUI_Components_Select = PS["App.GUI.Components.Select"];
  var App_GUI_Components_CrudButtons = PS["App.GUI.Components.CrudButtons"];
  var App_GUI_Components_Markup = PS["App.GUI.Components.Markup"];
  var App_GUI_Views_Crud = PS["App.GUI.Views.Crud"];
  var App_GUI_Views_Profiles = PS["App.GUI.Views.Profiles"];
  var App_Endpoint = PS["App.Endpoint"];
  var Endpoint_Client = PS["Endpoint.Client"];
  var Data_Serializable = PS["Data.Serializable"];
  var OpticUI_Core = PS["OpticUI.Core"];
  var Data_Generic = PS["Data.Generic"];
  var Data_Traversable = PS["Data.Traversable"];
  var Data_Lens_Getter = PS["Data.Lens.Getter"];
  var Data_Lens_Internal_Forget = PS["Data.Lens.Internal.Forget"];
  var Data_Lens_Prism_Maybe = PS["Data.Lens.Prism.Maybe"];        
  var Crud = (function () {
      function Crud(value0) {
          this.value0 = value0;
      };
      Crud.create = function (value0) {
          return new Crud(value0);
      };
      return Crud;
  })();
  var ToEvents = (function () {
      function ToEvents(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      ToEvents.create = function (value0) {
          return function (value1) {
              return new ToEvents(value0, value1);
          };
      };
      return ToEvents;
  })();
  var ToStatistics = (function () {
      function ToStatistics(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      ToStatistics.create = function (value0) {
          return function (value1) {
              return new ToStatistics(value0, value1);
          };
      };
      return ToStatistics;
  })();
  var updatePB = function (pb) {
      return Endpoint_Client.execEndpoint(Data_Serializable.serializableUnit)(App_Model_Photobooth.genericPhotobooth)(App_Model_Photobooth.genericPhotobooth)(App_Endpoint.putPhotobooths)(Prelude.unit)(pb);
  };
  var saveNewPb = function (pb) {
      return Endpoint_Client.execEndpoint(Data_Serializable.serializableUnit)(App_Model_Photobooth.genericPhotobooth)(App_Model_Photobooth.genericPhotobooth)(App_Endpoint.postPhotobooths)(Prelude.unit)(pb);
  };
  var makeNewPb = function (handle) {
      var c = function (model) {
          return function (h) {
              return App_GUI_Components_Markup.rowUI([ App_GUI_State._model(OpticUI_Core.uiStrong)(App_GUI_State._computername(OpticUI_Core.uiStrong)(OpticUI_Components.textField([ OpticUI_Markup_HTML.classA("form-control") ]))), App_GUI_State._model(OpticUI_Core.uiStrong)(App_GUI_State._alias(OpticUI_Core.uiStrong)(OpticUI_Components.textField([ OpticUI_Markup_HTML.classA("form-control") ]))), OpticUI_Core.ui(App_GUI_Components_Markup.emptyTd), App_GUI_State._state(OpticUI_Core.uiStrong)(App_GUI_Components_CrudButtons.newButton(function ($51) {
                  return handle(Crud.create($51));
              })), OpticUI_Core.ui(App_GUI_Components_Markup.emptyTd) ]);
          };
      };
      return OpticUI_Core["with"](c);
  };
  var loadPbs = Prelude[">>="](Control_Monad_Aff.bindAff)(Endpoint_Client.execEndpoint(Data_Serializable.serializableUnit)(Data_Generic.genericUnit)(Data_Generic.genericArray(App_Model_Photobooth.genericPhotobooth))(App_Endpoint.getPhotobooths)(Prelude.unit)(Prelude.unit))(function ($52) {
      return Prelude["return"](Control_Monad_Aff.applicativeAff)(App_Model_Photobooth.sortPhotobooths($52));
  });
  var linkButtons = function (handle) {
      return function (cn) {
          return function (alias) {
              return Prelude["<>"](OpticUI_Markup.markupSemigroup)(OpticUI_Markup_HTML.button([ OpticUI_Markup_HTML.classA("btn btn-primary"), OpticUI_Markup_HTML.onClick(function (v) {
                  return handle(new ToEvents(cn, alias));
              }) ])(OpticUI_Markup.text("Zie events")))(OpticUI_Markup_HTML.button([ OpticUI_Markup_HTML.classA("btn btn-primary"), OpticUI_Markup_HTML.onClick(function (v) {
                  return handle(new ToStatistics(cn, alias));
              }) ])(OpticUI_Markup.text("Zie statistieken")));
          };
      };
  };
  var showPB = function (handle) {
      return function (v) {
          return function (v1) {
              return function (v2) {
                  return function (editing) {
                      return function (i) {
                          if (v instanceof Data_Maybe.Just && i === v.value0) {
                              return OpticUI_Core["with"](function (v3) {
                                  return function (h) {
                                      return App_GUI_Components_Markup.rowUI([ OpticUI_Core.ui(OpticUI_Markup.text(v3.value0.computername)), App_Model_Photobooth._Photobooth(OpticUI_Core.uiStrong)(App_GUI_State._alias(OpticUI_Core.uiStrong)(OpticUI_Components.textField([ OpticUI_Markup_HTML.classA("form-control") ]))), App_Model_Photobooth._Photobooth(OpticUI_Core.uiStrong)(App_GUI_State._defaultprofile(OpticUI_Core.uiStrong)(App_GUI_Components_Select.select(Data_Maybe.fromMaybe([  ])(Data_StrMap.lookup(v3.value0.computername)(v2)))(Prelude.id(Prelude.categoryFn))([ OpticUI_Markup_HTML.classA("form-control") ]))), App_GUI_Components_CrudButtons.editButton(function ($53) {
                                          return handle(Crud.create($53));
                                      })(i)(editing), OpticUI_Core.ui(linkButtons(handle)(v3.value0.computername)(v3.value0.alias)) ]);
                                  };
                              });
                          };
                          return OpticUI_Core["with"](function (v3) {
                              return function (h) {
                                  return App_GUI_Components_Markup.rowUI(Prelude["<$>"](Prelude.functorArray)(OpticUI_Core.ui)([ OpticUI_Markup.text(v3.value0.computername), OpticUI_Markup.text(v3.value0.alias), OpticUI_Markup.text(v3.value0.defaultprofile), Data_Maybe.maybe(OpticUI_Markup_HTML.button([ OpticUI_Markup_HTML.classA("btn btn-action"), OpticUI_Markup_HTML.onClick(function (v4) {
                                      return handle(Crud.create(new App_GUI_Views_Crud.StartEdit(i)));
                                  }) ])(OpticUI_Markup.text("Edit")))(Prelude["const"](OpticUI_Markup.text("")))(v), linkButtons(handle)(v3.value0.computername)(v3.value0.alias), (function () {
                                      if (v1 instanceof Data_Maybe.Just && i === v1.value0) {
                                          return Prelude["<>"](OpticUI_Markup.markupSemigroup)(OpticUI_Markup_HTML.button([ OpticUI_Markup_HTML.classA("btn btn-danger"), OpticUI_Markup_HTML.onClick(function (v4) {
                                              return handle(Crud.create(new App_GUI_Views_Crud.Delete(v3)));
                                          }) ])(OpticUI_Markup.text("Sure?")))(OpticUI_Markup_HTML.button([ OpticUI_Markup_HTML.classA("btn btn-primary"), OpticUI_Markup_HTML.onClick(function (v4) {
                                              return handle(Crud.create(App_GUI_Views_Crud.CancelDelete.value));
                                          }) ])(OpticUI_Markup.text("Cancel!")));
                                      };
                                      if (v1 instanceof Data_Maybe.Just) {
                                          return OpticUI_Markup.text("");
                                      };
                                      if (v1 instanceof Data_Maybe.Nothing) {
                                          return OpticUI_Markup_HTML.button([ OpticUI_Markup_HTML.classA("btn btn-danger"), OpticUI_Markup_HTML.onClick(function (v4) {
                                              return handle(Crud.create(new App_GUI_Views_Crud.StartDelete(i)));
                                          }) ])(OpticUI_Markup.text("Delete"));
                                      };
                                      throw new Error("Failed pattern match at App.GUI.Views.PhotoboothsPage line 103, column 20 - line 109, column 18: " + [ v1.constructor.name ]);
                                  })() ]));
                              };
                          });
                      };
                  };
              };
          };
      };
  };
  var listPhotobooths = function (handle) {
      return function (selInd) {
          return function (delInd) {
              return function (profiles) {
                  var showColl = function (v) {
                      return function (v1) {
                          return function (h) {
                              if (v1 instanceof App_Model_Async.Initial) {
                                  return Prelude["<>"](OpticUI_Core.uiSemigroup(OpticUI_Markup.markupSemigroup))(OpticUI_Core.ui(OpticUI_Markup_HTML.tr([  ])(OpticUI_Markup_HTML.td([  ])(OpticUI_Markup.text("Nothing loaded yet")))))(App_Model_Async._Initial(OpticUI_Core.uiChoice(OpticUI_Markup.markupMonoid))(App_GUI_Components_Exec.exec(OpticUI_Markup.markupMonoid)(handle(new Crud(App_GUI_Views_Crud.LoadAll.value)))));
                              };
                              if (v1 instanceof App_Model_Async.Busy) {
                                  return Prelude["<>"](OpticUI_Core.uiSemigroup(OpticUI_Markup.markupSemigroup))(OpticUI_Core.ui(OpticUI_Markup_HTML.tr([  ])(OpticUI_Markup_HTML.td([  ])(OpticUI_Markup.text("Loading photobooths")))))(App_Model_Async._Busy(OpticUI_Core.uiChoice(OpticUI_Markup.markupMonoid))(OpticUI_Components_Async.onResult(OpticUI_Markup.markupMonoid)(function ($54) {
                                      return handle(Crud.create(App_GUI_Views_Crud.Loaded.create($54)));
                                  })(function ($55) {
                                      return handle(Crud.create(App_GUI_Views_Crud.LoadingFailed.create($55)));
                                  })));
                              };
                              if (v1 instanceof App_Model_Async.Done) {
                                  return App_Model_Async._Done(OpticUI_Core.uiChoice(OpticUI_Markup.markupMonoid))(OpticUI_Core.foreach(OpticUI_Markup.markupMonoid)(Data_Traversable.traversableArray)(showPB(handle)(selInd)(delInd)(profiles)(v)));
                              };
                              if (v1 instanceof App_Model_Async.Errored) {
                                  return OpticUI_Core.ui(OpticUI_Markup_HTML.tr([  ])(OpticUI_Markup_HTML.td([  ])(OpticUI_Markup.text("Photobooths failed to load: " + Control_Monad_Eff_Exception.message(v1.value0)))));
                              };
                              throw new Error("Failed pattern match at App.GUI.Views.PhotoboothsPage line 76, column 7 - line 78, column 7: " + [ v.constructor.name, v1.constructor.name, h.constructor.name ]);
                          };
                      };
                  };
                  return Data_Foldable.mconcat(Data_Foldable.foldableArray)(OpticUI_Core.uiMonoid(OpticUI_Markup.markupMonoid))([ OpticUI_Core["with"](function (s) {
                      return function (v) {
                          return App_GUI_State._collection(OpticUI_Core.uiStrong)(OpticUI_Core["with"](showColl(Data_Lens_Getter.view(App_GUI_State._editing(Data_Lens_Internal_Forget.strongForget))(s))));
                      };
                  }), App_GUI_State._editing(OpticUI_Core.uiStrong)(Data_Lens_Prism_Maybe._Just(OpticUI_Core.uiChoice(OpticUI_Markup.markupMonoid))(App_GUI_State._saving(OpticUI_Core.uiStrong)(App_Model_Async._Busy(OpticUI_Core.uiChoice(OpticUI_Markup.markupMonoid))(OpticUI_Components_Async.onResult(OpticUI_Markup.markupMonoid)(function ($56) {
                      return handle(Crud.create(App_GUI_Views_Crud.EditSaved.create($56)));
                  })(function ($57) {
                      return handle(Crud.create(App_GUI_Views_Crud.EditSaveFailed.create($57)));
                  }))))), App_GUI_State._deleting(OpticUI_Core.uiStrong)(Data_Lens_Prism_Maybe._Just(OpticUI_Core.uiChoice(OpticUI_Markup.markupMonoid))(App_GUI_State._saving(OpticUI_Core.uiStrong)(App_Model_Async._Busy(OpticUI_Core.uiChoice(OpticUI_Markup.markupMonoid))(OpticUI_Components_Async.onResult(OpticUI_Markup.markupMonoid)(function ($58) {
                      return handle(Prelude["const"](new Crud(App_GUI_Views_Crud.DeleteDone.value))($58));
                  })(function ($59) {
                      return handle(Crud.create(App_GUI_Views_Crud.DeleteFailed.create($59)));
                  }))))) ]);
              };
          };
      };
  };
  var deletePB = function (v) {
      return Endpoint_Client.execEndpoint(Data_Serializable.serializableString)(Data_Generic.genericUnit)(Data_Generic.genericUnit)(App_Endpoint.deletePhotobooth)(v.value0.computername)(Prelude.unit);
  };
  var photoboothsPage = function ($$goto) {
      return OpticUI_Core["with"](function (s) {
          return function (h) {
              var impls = {
                  loadAll: loadPbs, 
                  "delete": deletePB, 
                  saveNew: saveNewPb, 
                  saveEdit: updatePB, 
                  initial: Prelude["return"](Control_Monad_Eff.applicativeEff)({
                      id: Data_Maybe.Nothing.value, 
                      computername: "", 
                      alias: "", 
                      defaultprofile: ""
                  }), 
                  constr: App_Model_Photobooth.Photobooth.create
              };
              var handle = function (v) {
                  if (v instanceof Crud) {
                      return App_GUI_Views_Crud.crudHandler(s)(h)(impls)(v.value0);
                  };
                  if (v instanceof ToEvents) {
                      return $$goto(new App_GUI_State.EventsPage(v.value0, v.value1, 0));
                  };
                  if (v instanceof ToStatistics) {
                      return $$goto(new App_GUI_State.StatisticsPage(v.value0, v.value1));
                  };
                  throw new Error("Failed pattern match at App.GUI.Views.PhotoboothsPage line 50, column 3 - line 63, column 1: " + [ v.constructor.name ]);
              };
              return Prelude["<>"](OpticUI_Core.uiSemigroup(OpticUI_Markup.markupSemigroup))(OpticUI_Core.ui(App_GUI_Components_Markup.pageTitle(Prelude["<>"](OpticUI_Markup.markupSemigroup)(OpticUI_Markup_HTML.button([ OpticUI_Markup_HTML.classA("btn-nav is-home") ])(OpticUI_Markup.text("^")))(OpticUI_Markup.text(" Photobooths")))))(OpticUI_Core.withView(App_GUI_Components_Markup.crudTable)(Data_Foldable.mconcat(Data_Foldable.foldableArray)(OpticUI_Core.uiMonoid(OpticUI_Markup.markupMonoid))([ OpticUI_Core.ui(App_GUI_Components_Markup.tableHeader(Prelude.functorArray)(Data_Foldable.foldableArray)([  ])([ "Name", "Alias", "Default Profile", "Actions", "Link", "Delete" ])), App_GUI_State._new(OpticUI_Core.uiStrong)(makeNewPb(handle)), App_GUI_State._collectionEditingD(OpticUI_Core.uiStrong)(listPhotobooths(handle)(Prelude[">>="](Data_Maybe.bindMaybe)(Data_Lens_Getter.view(App_GUI_State._editing(Data_Lens_Internal_Forget.strongForget))(s))(function (ed) {
                  return Prelude["return"](Data_Maybe.applicativeMaybe)(ed.index);
              }))(Prelude[">>="](Data_Maybe.bindMaybe)(Data_Lens_Getter.view(App_GUI_State._deleting(Data_Lens_Internal_Forget.strongForget))(s))(function (d) {
                  return Prelude["return"](Data_Maybe.applicativeMaybe)(d.index);
              }))(Data_Lens_Getter.view(function ($60) {
                  return App_GUI_State._profiles(Data_Lens_Internal_Forget.strongForget)(App_Model_Async._Done(Data_Lens_Internal_Forget.choiceForget(Data_StrMap.monoidStrMap(Prelude.semigroupArray)))($60));
              })(s))), App_GUI_State._profiles(OpticUI_Core.uiStrong)(App_GUI_Views_Profiles.loadProfiles) ])));
          };
      });
  };
  exports["photoboothsPage"] = photoboothsPage;
})(PS["App.GUI.Views.PhotoboothsPage"] = PS["App.GUI.Views.PhotoboothsPage"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var OpticUI_Markup_HTML = PS["OpticUI.Markup.HTML"];
  var OpticUI_Markup = PS["OpticUI.Markup"];
  var OpticUI = PS["OpticUI"];
  var OpticUI_Components_Async = PS["OpticUI.Components.Async"];
  var Control_Monad_Eff_Exception = PS["Control.Monad.Eff.Exception"];
  var Control_Monad_Eff_Ref = PS["Control.Monad.Eff.Ref"];
  var Data_Lens = PS["Data.Lens"];
  var Data_Foldable = PS["Data.Foldable"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Monoid = PS["Data.Monoid"];
  var App_Model_Event = PS["App.Model.Event"];
  var App_Model_Statistic = PS["App.Model.Statistic"];
  var App_Model_Async = PS["App.Model.Async"];
  var App_Model_Date = PS["App.Model.Date"];
  var App_GUI_Types = PS["App.GUI.Types"];
  var App_GUI_State = PS["App.GUI.State"];
  var App_GUI_Components_Markup = PS["App.GUI.Components.Markup"];
  var App_GUI_Router = PS["App.GUI.Router"];
  var OpticUI_Core = PS["OpticUI.Core"];
  var Data_Lens_Setter = PS["Data.Lens.Setter"];
  var Data_Profunctor_Strong = PS["Data.Profunctor.Strong"];        
  var monthlyStatisticsLine = function (v) {
      return OpticUI_Markup_HTML.tr([  ])(Data_Foldable.mconcat(Data_Foldable.foldableArray)(OpticUI_Markup.markupMonoid)(Prelude["<$>"](Prelude.functorArray)(function ($30) {
          return OpticUI_Markup_HTML.td([  ])(OpticUI_Markup.text($30));
      })([ v.value0.computername + (" " + App_Model_Statistic.getMonthText(v.value0.month)), Prelude.show(Prelude.showInt)(v.value0.pictures), Prelude.show(Prelude.showInt)(v.value0.prints) ])));
  };
  var eventStatisticsLine = function (events) {
      return function (v) {
          return OpticUI_Markup_HTML.tr([  ])((function () {
              var $14 = Data_Foldable.find(Data_Foldable.foldableArray)(function (v1) {
                  return Data_Maybe.maybe(false)(function (i) {
                      return i === v.value0.eventId;
                  })(v1.value0.id);
              })(events);
              if ($14 instanceof Data_Maybe.Nothing) {
                  return OpticUI_Markup_HTML.td([  ])(OpticUI_Markup.text("No event found for statistic"));
              };
              if ($14 instanceof Data_Maybe.Just) {
                  return Data_Foldable.mconcat(Data_Foldable.foldableArray)(OpticUI_Markup.markupMonoid)(Prelude["<$>"](Prelude.functorArray)(function ($31) {
                      return OpticUI_Markup_HTML.td([  ])(OpticUI_Markup.text($31));
                  })([ $14.value0.value0.computername + (" " + ($14.value0.value0.name + (": Van " + (App_Model_Date.toLocalDatetime($14.value0.value0.datefrom) + (" tot " + App_Model_Date.toLocalDatetime($14.value0.value0.dateuntil)))))), Prelude.show(Prelude.showInt)(v.value0.pictures), Prelude.show(Prelude.showInt)(v.value0.prints) ]));
              };
              throw new Error("Failed pattern match at App.GUI.Views.StatisticsPage line 54, column 50 - line 61, column 10: " + [ $14.constructor.name ]);
          })());
      };
  };
  var statisticsPage = function (cn) {
      return function (nav) {
          var c = function (v) {
              return function (v1) {
                  if (v.statistics instanceof App_Model_Async.Done && v.events instanceof App_Model_Async.Done) {
                      return OpticUI_Core.ui(App_GUI_Components_Markup.crudTable(Data_Foldable.mconcat(Data_Foldable.foldableArray)(OpticUI_Markup.markupMonoid)(Prelude["<>"](Prelude.semigroupArray)([ App_GUI_Components_Markup.tableHeader(Prelude.functorArray)(Data_Foldable.foldableArray)([  ])([ "Classificatie", "Fotos", "Prints" ]) ])(Prelude["<>"](Prelude.semigroupArray)(Prelude.map(Prelude.functorArray)(monthlyStatisticsLine)(v.statistics.value0.value0.monthlyStatistics))(Prelude.map(Prelude.functorArray)(eventStatisticsLine(v.events.value0))(v.statistics.value0.value0.eventStatistics))))));
                  };
                  return Data_Monoid.mempty(OpticUI_Core.uiMonoid(OpticUI_Markup.markupMonoid));
              };
          };
          return Prelude["<>"](OpticUI_Core.uiSemigroup(OpticUI_Markup.markupSemigroup))(OpticUI_Core.ui(App_GUI_Components_Markup.pageTitle(Prelude["<>"](OpticUI_Markup.markupSemigroup)(OpticUI_Markup_HTML.button([ OpticUI_Markup_HTML.classA("btn-nav"), OpticUI_Markup_HTML.onClick(function (v) {
              return nav(App_GUI_State.PhotoboothsPage.value);
          }) ])(OpticUI_Markup.text("^")))(Prelude["<>"](OpticUI_Markup.markupSemigroup)(OpticUI_Markup.text(" Statistics for: "))(OpticUI_Markup_HTML.em([  ])(OpticUI_Markup.text(cn)))))))(Prelude["<>"](OpticUI_Core.uiSemigroup(OpticUI_Markup.markupSemigroup))(OpticUI_Core["with"](function (s) {
              return function (h) {
                  return Data_Foldable.mconcat(Data_Foldable.foldableArray)(OpticUI_Core.uiMonoid(OpticUI_Markup.markupMonoid))([ App_GUI_State._statistics(OpticUI_Core.uiStrong)(App_Model_Async._Busy(OpticUI_Core.uiChoice(OpticUI_Markup.markupMonoid))(Prelude["<>"](OpticUI_Core.uiSemigroup(OpticUI_Markup.markupSemigroup))(OpticUI_Components_Async.onResult(OpticUI_Markup.markupMonoid)(function (a) {
                      return OpticUI_Core.runHandler(h)(Data_Lens_Setter.set(App_GUI_State._statistics(Data_Profunctor_Strong.strongFn))(new App_Model_Async.Done(a))(s));
                  })(function (err) {
                      return OpticUI_Core.runHandler(h)(Data_Lens_Setter.set(App_GUI_State._statistics(Data_Profunctor_Strong.strongFn))(new App_Model_Async.Errored(err))(s));
                  }))(OpticUI_Core.ui(OpticUI_Markup_HTML.div([  ])(OpticUI_Markup.text("Loading statistics")))))), App_GUI_State._statistics(OpticUI_Core.uiStrong)(App_Model_Async._Errored(OpticUI_Core.uiChoice(OpticUI_Markup.markupMonoid))(OpticUI_Core["with"](function (err) {
                      return function (v) {
                          return OpticUI_Core.ui(OpticUI_Markup_HTML.div([ OpticUI_Markup_HTML.classA("alert alert-danger") ])(OpticUI_Markup.text("Statistics failed to load: " + Control_Monad_Eff_Exception.message(err))));
                      };
                  }))), App_GUI_State._events(OpticUI_Core.uiStrong)(App_Model_Async._Busy(OpticUI_Core.uiChoice(OpticUI_Markup.markupMonoid))(Prelude["<>"](OpticUI_Core.uiSemigroup(OpticUI_Markup.markupSemigroup))(OpticUI_Components_Async.onResult(OpticUI_Markup.markupMonoid)(function (a) {
                      return OpticUI_Core.runHandler(h)(Data_Lens_Setter.set(App_GUI_State._events(Data_Profunctor_Strong.strongFn))(new App_Model_Async.Done(a))(s));
                  })(function (err) {
                      return OpticUI_Core.runHandler(h)(Data_Lens_Setter.set(App_GUI_State._events(Data_Profunctor_Strong.strongFn))(new App_Model_Async.Errored(err))(s));
                  }))(OpticUI_Core.ui(OpticUI_Markup_HTML.div([  ])(OpticUI_Markup.text("Loading events")))))), App_GUI_State._events(OpticUI_Core.uiStrong)(App_Model_Async._Errored(OpticUI_Core.uiChoice(OpticUI_Markup.markupMonoid))(OpticUI_Core["with"](function (err) {
                      return function (v) {
                          return OpticUI_Core.ui(OpticUI_Markup_HTML.div([ OpticUI_Markup_HTML.classA("alert alert-danger") ])(OpticUI_Markup.text("Events failed to load: " + Control_Monad_Eff_Exception.message(err))));
                      };
                  }))) ]);
              };
          }))(OpticUI_Core["with"](c)));
      };
  };
  exports["eventStatisticsLine"] = eventStatisticsLine;
  exports["monthlyStatisticsLine"] = monthlyStatisticsLine;
  exports["statisticsPage"] = statisticsPage;
})(PS["App.GUI.Views.StatisticsPage"] = PS["App.GUI.Views.StatisticsPage"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];        
  var when = function (dictMonad) {
      return function (v) {
          return function (v1) {
              if (v) {
                  return v1;
              };
              if (!v) {
                  return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(Prelude.unit);
              };
              throw new Error("Failed pattern match at Control.Monad line 9, column 1 - line 10, column 1: " + [ v.constructor.name, v1.constructor.name ]);
          };
      };
  };
  exports["when"] = when;
})(PS["Control.Monad"] = PS["Control.Monad"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  // module DOM.Event.EventTarget

  exports.eventListener = function (fn) {
    return function (event) {
      return fn(event)();
    };
  };

  exports.addEventListener = function (type) {
    return function (listener) {
      return function (useCapture) {
        return function (target) {
          return function () {
            target.addEventListener(type, listener, useCapture);
            return {};
          };
        };
      };
    };
  };
})(PS["DOM.Event.EventTarget"] = PS["DOM.Event.EventTarget"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["DOM.Event.EventTarget"];
  var Prelude = PS["Prelude"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Control_Monad_Eff_Exception = PS["Control.Monad.Eff.Exception"];
  var DOM = PS["DOM"];
  var DOM_Event_Types = PS["DOM.Event.Types"];
  exports["addEventListener"] = $foreign.addEventListener;
  exports["eventListener"] = $foreign.eventListener;
})(PS["DOM.Event.EventTarget"] = PS["DOM.Event.EventTarget"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var DOM_Event_Types = PS["DOM.Event.Types"];
  var load = "load";
  exports["load"] = load;
})(PS["DOM.Event.EventTypes"] = PS["DOM.Event.EventTypes"] || {});
(function(exports) {
  /* global exports, window */
  "use strict";

  // module DOM.HTML

  exports.window = function () {
    return window;
  };
})(PS["DOM.HTML"] = PS["DOM.HTML"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["DOM.HTML"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var DOM = PS["DOM"];
  var DOM_HTML_Types = PS["DOM.HTML.Types"];
  exports["window"] = $foreign.window;
})(PS["DOM.HTML"] = PS["DOM.HTML"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  // module DOM.HTML.Document

  exports.body = function (doc) {
    return function () {
      return doc.body;
    };
  };
})(PS["DOM.HTML.Document"] = PS["DOM.HTML.Document"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["DOM.HTML.Document"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Data_Nullable = PS["Data.Nullable"];
  var DOM = PS["DOM"];
  var DOM_HTML_Types = PS["DOM.HTML.Types"];
  exports["body"] = $foreign.body;
})(PS["DOM.HTML.Document"] = PS["DOM.HTML.Document"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  // module DOM.HTML.Window

  exports.document = function (window) {
    return function () {
      return window.document;
    };
  };
})(PS["DOM.HTML.Window"] = PS["DOM.HTML.Window"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["DOM.HTML.Window"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var DOM = PS["DOM"];
  var DOM_HTML_Types = PS["DOM.HTML.Types"];
  exports["document"] = $foreign.document;
})(PS["DOM.HTML.Window"] = PS["DOM.HTML.Window"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  exports.appendChild = function (node) {
    return function (parent) {
      return function () {
        return parent.appendChild(node);
      };
    };
  };
})(PS["DOM.Node.Node"] = PS["DOM.Node.Node"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var $foreign = PS["DOM.Node.Node"];
  var Prelude = PS["Prelude"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Data_Enum = PS["Data.Enum"];
  var Data_Nullable = PS["Data.Nullable"];
  var Data_Maybe_Unsafe = PS["Data.Maybe.Unsafe"];
  var DOM = PS["DOM"];
  var DOM_Node_NodeType = PS["DOM.Node.NodeType"];
  var DOM_Node_Types = PS["DOM.Node.Types"];
  exports["appendChild"] = $foreign.appendChild;
})(PS["DOM.Node.Node"] = PS["DOM.Node.Node"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var Prelude = PS["Prelude"];
  var OpticUI_Internal_VirtualDOM = PS["OpticUI.Internal.VirtualDOM"];
  var OpticUI_Markup = PS["OpticUI.Markup"];
  var OpticUI_Core = PS["OpticUI.Core"];
  var Control_Monad = PS["Control.Monad"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Control_Monad_Eff_Ref = PS["Control.Monad.Eff.Ref"];
  var Control_Monad_State = PS["Control.Monad.State"];
  var Control_Monad_State_Class = PS["Control.Monad.State.Class"];
  var Data_Function = PS["Data.Function"];
  var Data_Exists = PS["Data.Exists"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Either = PS["Data.Either"];
  var Data_Nullable = PS["Data.Nullable"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_Tuple = PS["Data.Tuple"];
  var Data_Traversable = PS["Data.Traversable"];
  var Data_Array = PS["Data.Array"];
  var Data_StrMap = PS["Data.StrMap"];
  var DOM = PS["DOM"];
  var DOM_Event_EventTarget = PS["DOM.Event.EventTarget"];
  var DOM_Event_EventTypes = PS["DOM.Event.EventTypes"];
  var DOM_HTML = PS["DOM.HTML"];
  var DOM_HTML_Document = PS["DOM.HTML.Document"];
  var DOM_Node_Element = PS["DOM.Node.Element"];
  var DOM_HTML_Types = PS["DOM.HTML.Types"];
  var DOM_HTML_Window = PS["DOM.HTML.Window"];
  var DOM_Node_Node = PS["DOM.Node.Node"];
  var Control_Monad_State_Trans = PS["Control.Monad.State.Trans"];
  var Data_Identity = PS["Data.Identity"];        
  var onLoad = function (go) {
      return function __do() {
          var v = DOM_HTML.window();
          var et = DOM_HTML_Types.windowToEventTarget(v);
          return DOM_Event_EventTarget.addEventListener(DOM_Event_EventTypes.load)(DOM_Event_EventTarget.eventListener(function (v1) {
              return go;
          }))(false)(et)();
      };
  };
  var findProp = function (getter) {
      return function (setter) {
          return function (key) {
              return function (newprop) {
                  return Prelude.bind(Control_Monad_State_Trans.bindStateT(Data_Identity.monadIdentity))(Control_Monad_State_Class.gets(Control_Monad_State_Trans.monadStateStateT(Data_Identity.monadIdentity))(getter))(function (v) {
                      var $36 = Data_StrMap.lookup(key)(v);
                      if ($36 instanceof Data_Maybe.Nothing) {
                          return Prelude.bind(Control_Monad_State_Trans.bindStateT(Data_Identity.monadIdentity))(Control_Monad_State_Class.modify(Control_Monad_State_Trans.monadStateStateT(Data_Identity.monadIdentity))(setter(Data_StrMap.insert(key)(newprop)(v))))(function () {
                              return Prelude["return"](Control_Monad_State_Trans.applicativeStateT(Data_Identity.monadIdentity))(newprop);
                          });
                      };
                      if ($36 instanceof Data_Maybe.Just) {
                          return Prelude["return"](Control_Monad_State_Trans.applicativeStateT(Data_Identity.monadIdentity))($36.value0);
                      };
                      throw new Error("Failed pattern match at OpticUI.Run line 111, column 3 - line 119, column 1: " + [ $36.constructor.name ]);
                  });
              };
          };
      };
  };
  var toVProp = function (v) {
      if (v instanceof OpticUI_Markup.AttrP) {
          return Prelude.pure(Control_Monad_State_Trans.applicativeStateT(Data_Identity.monadIdentity))(OpticUI_Internal_VirtualDOM.attrProp(v.value0, v.value1));
      };
      if (v instanceof OpticUI_Markup.HandlerP) {
          return Prelude.pure(Control_Monad_State_Trans.applicativeStateT(Data_Identity.monadIdentity))(OpticUI_Markup.runEventHandler(function (f) {
              return OpticUI_Internal_VirtualDOM.handlerProp(v.value0, f);
          })(v.value1));
      };
      if (v instanceof OpticUI_Markup.PropP) {
          return Prelude.pure(Control_Monad_State_Trans.applicativeStateT(Data_Identity.monadIdentity))(Data_Exists.runExists(function (v1) {
              return OpticUI_Internal_VirtualDOM.prop(v.value0, v1.value0);
          })(v.value1));
      };
      if (v instanceof OpticUI_Markup.InitializerP) {
          return findProp(function (v1) {
              return v1.initializers;
          })(function (is) {
              return function (v1) {
                  var $47 = {};
                  for (var $48 in v1) {
                      if (v1.hasOwnProperty($48)) {
                          $47[$48] = v1[$48];
                      };
                  };
                  $47.initializers = is;
                  return $47;
              };
          })(v.value0)(OpticUI_Markup.runInitializer(function (i) {
              return OpticUI_Internal_VirtualDOM.initializer(v.value0, i);
          })(v.value1));
      };
      if (v instanceof OpticUI_Markup.FinalizerP) {
          return findProp(function (v1) {
              return v1.finalizers;
          })(function (fs) {
              return function (v1) {
                  var $51 = {};
                  for (var $52 in v1) {
                      if (v1.hasOwnProperty($52)) {
                          $51[$52] = v1[$52];
                      };
                  };
                  $51.finalizers = fs;
                  return $51;
              };
          })(v.value0)(OpticUI_Markup.runFinalizer(function (i) {
              return OpticUI_Internal_VirtualDOM.finalizer(v.value0, i);
          })(v.value1));
      };
      throw new Error("Failed pattern match at OpticUI.Run line 93, column 1 - line 94, column 1: " + [ v.constructor.name ]);
  };
  var toVTree = function (v) {
      if (v instanceof OpticUI_Markup.Text) {
          return Prelude["return"](Control_Monad_State_Trans.applicativeStateT(Data_Identity.monadIdentity))(OpticUI_Internal_VirtualDOM.vtext(v.value0));
      };
      if (v instanceof OpticUI_Markup.Element) {
          return Prelude.bind(Control_Monad_State_Trans.bindStateT(Data_Identity.monadIdentity))(Data_Array.foldM(Control_Monad_State_Trans.monadStateT(Data_Identity.monadIdentity))(function (acc) {
              return function (prop) {
                  return Prelude["<$>"](Control_Monad_State_Trans.functorStateT(Data_Identity.monadIdentity))(Prelude.append(OpticUI_Internal_VirtualDOM.semigroupProps)(acc))(toVProp(prop));
              };
          })(Data_Monoid.mempty(OpticUI_Internal_VirtualDOM.monoidProps))(v.value2))(function (v1) {
              return Prelude.bind(Control_Monad_State_Trans.bindStateT(Data_Identity.monadIdentity))(Data_Traversable.traverse(Data_Traversable.traversableArray)(Control_Monad_State_Trans.applicativeStateT(Data_Identity.monadIdentity))(toVTree)(v.value3.value0))(function (v2) {
                  return Prelude["return"](Control_Monad_State_Trans.applicativeStateT(Data_Identity.monadIdentity))(OpticUI_Internal_VirtualDOM.vnode(Data_Nullable.toNullable(v.value0))(v.value1)(Data_Nullable.toNullable(Data_Maybe.Nothing.value))(v1)(v2));
              });
          });
      };
      throw new Error("Failed pattern match at OpticUI.Run line 86, column 1 - line 87, column 1: " + [ v.constructor.name ]);
  };
  var buildVTree = function (v) {
      return function (memo) {
          var n = Data_Nullable.toNullable(Data_Maybe.Nothing.value);
          var $66 = Control_Monad_State.runState(Data_Traversable.traverse(Data_Traversable.traversableArray)(Control_Monad_State_Trans.applicativeStateT(Data_Identity.monadIdentity))(toVTree)(v.value0))(memo);
          return new Data_Tuple.Tuple(OpticUI_Internal_VirtualDOM.vnode(n)("div")(n)(Data_Monoid.mempty(OpticUI_Internal_VirtualDOM.monoidProps))($66.value0), $66.value1);
      };
  };
  var appendToBody = function (e) {
      return function __do() {
          var v = DOM_HTML.window();
          var v1 = DOM_HTML_Window.document(v)();
          var v2 = Prelude["<$>"](Control_Monad_Eff.functorEff)(Data_Nullable.toMaybe)(DOM_HTML_Document.body(v1))();
          if (v2 instanceof Data_Maybe.Nothing) {
              return Prelude.unit;
          };
          if (v2 instanceof Data_Maybe.Just) {
              return Prelude["void"](Control_Monad_Eff.functorEff)(DOM_Node_Node.appendChild(DOM_HTML_Types.htmlElementToNode(e))(DOM_HTML_Types.htmlElementToNode(v2.value0)))();
          };
          throw new Error("Failed pattern match at OpticUI.Run line 130, column 3 - line 132, column 78: " + [ v2.constructor.name ]);
      };
  };
  var animate = function (s0) {
      return function (ui) {
          var v0 = OpticUI_Internal_VirtualDOM.vtext("");
          var n0 = OpticUI_Internal_VirtualDOM.createElement(v0);
          return function __do() {
              var v = Control_Monad_Eff_Ref.newRef(v0)();
              var v1 = Control_Monad_Eff_Ref.newRef(n0)();
              var v2 = Control_Monad_Eff_Ref.newRef(s0)();
              var v3 = Control_Monad_Eff_Ref.newRef(0)();
              var v4 = Control_Monad_Eff_Ref.newRef({
                  initializers: Data_StrMap.empty, 
                  finalizers: Data_StrMap.empty
              })();
              var checkGen = function (g) {
                  return function (go) {
                      return function __do() {
                          var v5 = Control_Monad_Eff_Ref.readRef(v3)();
                          return Control_Monad.when(Control_Monad_Eff.monadEff)(g === v5)(function __do() {
                              Control_Monad_Eff_Ref.writeRef(v3)(v5 + 1 | 0)();
                              return go();
                          })();
                      };
                  };
              };
              var step = function (gen) {
                  return function (s) {
                      return checkGen(gen)(function __do() {
                          var v5 = Control_Monad_Eff_Ref.readRef(v)();
                          var v7 = Control_Monad_Eff_Ref.readRef(v4)();
                          var v8 = Prelude["<$>"](Control_Monad_Eff.functorEff)(function (tree) {
                              return buildVTree(tree)(v7);
                          })(OpticUI_Core.runUI(ui)(s)(OpticUI_Core.Handler(step(gen + 1 | 0))))();
                          var v9 = Control_Monad_Eff_Ref.writeRef(v4)(v8.value1)();
                          var v10 = Control_Monad_Eff_Ref.writeRef(v)(v8.value0)();
                          var v11 = Control_Monad_Eff_Ref.writeRef(v2)(s)();
                          var v12 = Control_Monad_Eff_Ref.readRef(v1)();
                          var v13 = OpticUI_Internal_VirtualDOM.patch(OpticUI_Internal_VirtualDOM.diff(v5)(v8.value0))(v12)();
                          return Control_Monad_Eff_Ref.writeRef(v1)(v13)();
                      });
                  };
              };
              var driver = function (f) {
                  return function __do() {
                      var v5 = Control_Monad_Eff_Ref.readRef(v3)();
                      var v6 = Control_Monad_Eff_Ref.readRef(v2)();
                      var v7 = f(v6)();
                      return step(v5)(v7)();
                  };
              };
              onLoad(function __do() {
                  appendToBody(n0)();
                  return step(0)(s0)();
              })();
              return driver;
          };
      };
  };
  exports["appendToBody"] = appendToBody;
  exports["onLoad"] = onLoad;
  exports["findProp"] = findProp;
  exports["toVProp"] = toVProp;
  exports["toVTree"] = toVTree;
  exports["buildVTree"] = buildVTree;
  exports["animate"] = animate;
})(PS["OpticUI.Run"] = PS["OpticUI.Run"] || {});
(function(exports) {
  // Generated by psc version 0.8.5.0
  "use strict";
  var App_GUI_Router = PS["App.GUI.Router"];
  var App_GUI_State = PS["App.GUI.State"];
  var App_GUI_Types = PS["App.GUI.Types"];
  var App_GUI_Views_EventsPage = PS["App.GUI.Views.EventsPage"];
  var App_GUI_Views_LoginPage = PS["App.GUI.Views.LoginPage"];
  var App_GUI_Views_PhotoboothsPage = PS["App.GUI.Views.PhotoboothsPage"];
  var App_GUI_Views_StatisticsPage = PS["App.GUI.Views.StatisticsPage"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Data_Lens = PS["Data.Lens"];
  var Data_Maybe = PS["Data.Maybe"];
  var OpticUI = PS["OpticUI"];
  var Prelude = PS["Prelude"];
  var Data_Lens_Setter = PS["Data.Lens.Setter"];
  var Data_Profunctor_Strong = PS["Data.Profunctor.Strong"];
  var OpticUI_Run = PS["OpticUI.Run"];
  var OpticUI_Core = PS["OpticUI.Core"];
  var Data_Lens_Getter = PS["Data.Lens.Getter"];
  var Data_Lens_Internal_Forget = PS["Data.Lens.Internal.Forget"];        
  var main = function __do() {
      var v = App_GUI_State.initialState(App_GUI_State.PhotoboothsPage.value)();
      var v1 = Prelude["<$>"](Control_Monad_Eff.functorEff)(App_GUI_Router.match)(App_GUI_Router.getHash)();
      var v2 = App_GUI_Router.resolve(v)(v1)();
      var newSWithRoute = Data_Lens_Setter.set(App_GUI_State._route(Data_Profunctor_Strong.strongFn))(v1)(v2);
      var v3 = OpticUI_Run.animate(newSWithRoute)(OpticUI_Core["with"](function (s) {
          return function (h) {
              var nav$prime = App_GUI_Router.nav(s)(h);
              var $8 = Data_Maybe.isNothing(s.session);
              if ($8) {
                  return App_GUI_State._loginPage(OpticUI_Core.uiStrong)(App_GUI_Views_LoginPage.loginPage(nav$prime));
              };
              if (!$8) {
                  var $9 = Data_Lens_Getter.view(App_GUI_State._route(Data_Lens_Internal_Forget.strongForget))(s);
                  if ($9 instanceof App_GUI_State.LoginPage) {
                      return App_GUI_State._loginPage(OpticUI_Core.uiStrong)(App_GUI_Views_LoginPage.loginPage(nav$prime));
                  };
                  if ($9 instanceof App_GUI_State.PhotoboothsPage) {
                      return App_GUI_State._pbPage(OpticUI_Core.uiStrong)(App_GUI_Views_PhotoboothsPage.photoboothsPage(nav$prime));
                  };
                  if ($9 instanceof App_GUI_State.EventsPage) {
                      return App_GUI_State._eventsPage(OpticUI_Core.uiStrong)(App_GUI_Views_EventsPage.eventsPage($9.value0)($9.value1)($9.value2)(nav$prime));
                  };
                  if ($9 instanceof App_GUI_State.StatisticsPage) {
                      return App_GUI_State._statisticsPage(OpticUI_Core.uiStrong)(App_GUI_Views_StatisticsPage.statisticsPage($9.value1)(nav$prime));
                  };
                  throw new Error("Failed pattern match at App.GUI line 29, column 17 - line 34, column 3: " + [ $9.constructor.name ]);
              };
              throw new Error("Failed pattern match at App.GUI line 27, column 9 - line 34, column 3: " + [ $8.constructor.name ]);
          };
      }))();
      return App_GUI_Router.hashChanged(function (str) {
          return v3(function (s) {
              var newRoute = App_GUI_Router.match(str);
              return function __do() {
                  var v4 = App_GUI_Router.resolve(s)(newRoute)();
                  return Data_Lens_Setter.set(App_GUI_State._route(Data_Profunctor_Strong.strongFn))(newRoute)(v4);
              };
          });
      })();
  };
  exports["main"] = main;
})(PS["App.GUI"] = PS["App.GUI"] || {});
PS["App.GUI"].main();

},{"virtual-dom/create-element":3,"virtual-dom/diff":4,"virtual-dom/patch":5,"virtual-dom/virtual-hyperscript/hooks/soft-set-hook":12,"virtual-dom/vnode/vnode":20,"virtual-dom/vnode/vtext":22}],27:[function(require,module,exports){

},{}]},{},[26]);
