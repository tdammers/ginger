/**
 * Ginger's JavaScript prelude.
 */

var $prelude = (function () {
    var defaultContext = {
    }

    defaultContext.print = console.log

    var fold = function (f, a0, xs) {
        var acc = a0
        for (var x of xs) {
            acc = f(acc, x)
        }
        return acc
    }

    var fold1 = function (f, xs) {
        var a0 = xs[0]
        return fold(f, a0, xs)
    }

    defaultContext.sum =
        function () { return fold ((a, b) => a + b, 0, arguments) }
    defaultContext.difference =
        function () { return fold ((a, b) => a - b, 0, arguments) }
    defaultContext.product =
        function () { return fold ((a, b) => a * b, 1, arguments) }
    defaultContext.ratio =
        function () { return fold1 ((a, b) => a / b, arguments) }
    defaultContext.int_ratio =
        function () { return fold1 ((a, b) => Math.floor(a / b), arguments) }
    defaultContext.modulo =
        function () { return fold1 ((a, b) => a % b, arguments) }
    defaultContext.concat =
        function () { return fold ((a, b) => String(a) + String(b), '', arguments) }
    defaultContext.abs = Math.abs
    defaultContext.ceil = Math.ceil
    defaultContext.floor = Math.floor
    defaultContext.round = Math.round
    defaultContext.truncate = function (x) {
        return Math.sign(x) * Math.floor(Math.abs(x))
    }
    defaultContext.urlencode = encodeURI
    defaultContext.capitalize = function (str) {
            ((str || '') + '').replace(/(?:^[a-z])[a-z]/g, function(x) { return x.toUpperCase })
        }
    defaultContext.center = function (str) {
        // TODO
        return str
    }
    defaultContext.default = function (value, def) {
        return value || def
    }
    defaultContext.raw = function (a) { return { __html: a } }
    defaultContext.escape = function (a) {
        if (!a) return null
        if (a.__html) { return a } else { return { __html: htmlencode(a) } }
    }
    defaultContext.filesizeformat = function (a) {
        // TODO
        return a
    }
    defaultContext.filter = function (items, predicate) {
        var result = []
        var predicate = predicate || function (x) { return true }
        iterate(items,
            function (k, x) {
                if (predicate(x)) {
                    result.push(x)
                }
            })
        return result
    }
    defaultContext.sort = function (items) {
        return items.sort()
    }
    defaultContext.slice = function (items, a, b) {
        return items.slice(a, b)
    }
    defaultContext.int = function (x) {
        return Math.floor(x)
    }
    defaultContext.str = function (x) {
        return '' + x
    }
    defaultContext.length = function (x) {
        return x.length || 0
    }
    defaultContext.printf = function () {
        var args = [].slice.call(arguments)
        var fmt = args.shift()
        return fmt
    }

    var htmlencode = function (val) {
        encodedHtml = escape(val);
        encodedHtml = encodedHtml.replace(/\//g,"%2F");
        encodedHtml = encodedHtml.replace(/\?/g,"%3F");
        encodedHtml = encodedHtml.replace(/=/g,"%3D");
        encodedHtml = encodedHtml.replace(/&/g,"%26");
        encodedHtml = encodedHtml.replace(/@/g,"%40");
        return encodedHtml
    }

    var mergeObjects = function (a, b) {
        var c = {}
        var k = null
        for (k in a) { c[k] = a[k] }
        for (k in b) { c[k] = b[k] }
        return c
    }

    var lookup = function (c, k) {
        if (typeof(c) === 'object') {
            return c[k]
        }
        else {
            return null
        }
    }

    var iterate = function (iteree, fn) {
        if (typeof(iteree) !== 'object') {
            return null
        }
        if (Array.isArray(iteree)) {
            for (var i = 0; i < iteree.length; ++i) {
                fn(i, iteree[i])
            }
        }
        else {
            for (var k in iteree) {
                fn(k, iteree[k])
            }
        }
    }

    var callFunction = function (fn, args) {
        if (typeof(fn) === 'function') {
            fn.apply(args)
        }
        else {
            return null
        }
    }

    return {
        defaultContext: defaultContext,
        mergeObjects: mergeObjects,
        lookup: lookup,
        iterate: iterate,
        callFunction: callFunction
    }
})()
