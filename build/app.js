
(function() {
'use strict';

function F2(fun)
{
  function wrapper(a) { return function(b) { return fun(a,b); }; }
  wrapper.arity = 2;
  wrapper.func = fun;
  return wrapper;
}

function F3(fun)
{
  function wrapper(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  }
  wrapper.arity = 3;
  wrapper.func = fun;
  return wrapper;
}

function F4(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  }
  wrapper.arity = 4;
  wrapper.func = fun;
  return wrapper;
}

function F5(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  }
  wrapper.arity = 5;
  wrapper.func = fun;
  return wrapper;
}

function F6(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  }
  wrapper.arity = 6;
  wrapper.func = fun;
  return wrapper;
}

function F7(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  }
  wrapper.arity = 7;
  wrapper.func = fun;
  return wrapper;
}

function F8(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  }
  wrapper.arity = 8;
  wrapper.func = fun;
  return wrapper;
}

function F9(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  }
  wrapper.arity = 9;
  wrapper.func = fun;
  return wrapper;
}

function A2(fun, a, b)
{
  return fun.arity === 2
    ? fun.func(a, b)
    : fun(a)(b);
}
function A3(fun, a, b, c)
{
  return fun.arity === 3
    ? fun.func(a, b, c)
    : fun(a)(b)(c);
}
function A4(fun, a, b, c, d)
{
  return fun.arity === 4
    ? fun.func(a, b, c, d)
    : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e)
{
  return fun.arity === 5
    ? fun.func(a, b, c, d, e)
    : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f)
{
  return fun.arity === 6
    ? fun.func(a, b, c, d, e, f)
    : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g)
{
  return fun.arity === 7
    ? fun.func(a, b, c, d, e, f, g)
    : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h)
{
  return fun.arity === 8
    ? fun.func(a, b, c, d, e, f, g, h)
    : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i)
{
  return fun.arity === 9
    ? fun.func(a, b, c, d, e, f, g, h, i)
    : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

//import Native.Utils //

var _elm_lang$core$Native_Basics = function() {

function div(a, b)
{
	return (a / b) | 0;
}
function rem(a, b)
{
	return a % b;
}
function mod(a, b)
{
	if (b === 0)
	{
		throw new Error('Cannot perform mod 0. Division by zero error.');
	}
	var r = a % b;
	var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r + b) : -mod(-a, -b));

	return m === b ? 0 : m;
}
function logBase(base, n)
{
	return Math.log(n) / Math.log(base);
}
function negate(n)
{
	return -n;
}
function abs(n)
{
	return n < 0 ? -n : n;
}

function min(a, b)
{
	return _elm_lang$core$Native_Utils.cmp(a, b) < 0 ? a : b;
}
function max(a, b)
{
	return _elm_lang$core$Native_Utils.cmp(a, b) > 0 ? a : b;
}
function clamp(lo, hi, n)
{
	return _elm_lang$core$Native_Utils.cmp(n, lo) < 0
		? lo
		: _elm_lang$core$Native_Utils.cmp(n, hi) > 0
			? hi
			: n;
}

var ord = ['LT', 'EQ', 'GT'];

function compare(x, y)
{
	return { ctor: ord[_elm_lang$core$Native_Utils.cmp(x, y) + 1] };
}

function xor(a, b)
{
	return a !== b;
}
function not(b)
{
	return !b;
}
function isInfinite(n)
{
	return n === Infinity || n === -Infinity;
}

function truncate(n)
{
	return n | 0;
}

function degrees(d)
{
	return d * Math.PI / 180;
}
function turns(t)
{
	return 2 * Math.PI * t;
}
function fromPolar(point)
{
	var r = point._0;
	var t = point._1;
	return _elm_lang$core$Native_Utils.Tuple2(r * Math.cos(t), r * Math.sin(t));
}
function toPolar(point)
{
	var x = point._0;
	var y = point._1;
	return _elm_lang$core$Native_Utils.Tuple2(Math.sqrt(x * x + y * y), Math.atan2(y, x));
}

return {
	div: F2(div),
	rem: F2(rem),
	mod: F2(mod),

	pi: Math.PI,
	e: Math.E,
	cos: Math.cos,
	sin: Math.sin,
	tan: Math.tan,
	acos: Math.acos,
	asin: Math.asin,
	atan: Math.atan,
	atan2: F2(Math.atan2),

	degrees: degrees,
	turns: turns,
	fromPolar: fromPolar,
	toPolar: toPolar,

	sqrt: Math.sqrt,
	logBase: F2(logBase),
	negate: negate,
	abs: abs,
	min: F2(min),
	max: F2(max),
	clamp: F3(clamp),
	compare: F2(compare),

	xor: F2(xor),
	not: not,

	truncate: truncate,
	ceiling: Math.ceil,
	floor: Math.floor,
	round: Math.round,
	toFloat: function(x) { return x; },
	isNaN: isNaN,
	isInfinite: isInfinite
};

}();
//import //

var _elm_lang$core$Native_Utils = function() {

// COMPARISONS

function eq(x, y)
{
	var stack = [];
	var isEqual = eqHelp(x, y, 0, stack);
	var pair;
	while (isEqual && (pair = stack.pop()))
	{
		isEqual = eqHelp(pair.x, pair.y, 0, stack);
	}
	return isEqual;
}


function eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push({ x: x, y: y });
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object')
	{
		if (typeof x === 'function')
		{
			throw new Error(
				'Trying to use `(==)` on functions. There is no way to know if functions are "the same" in the Elm sense.'
				+ ' Read more about this at http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#=='
				+ ' which describes why it is this way and what the better version will look like.'
			);
		}
		return false;
	}

	if (x === null || y === null)
	{
		return false
	}

	if (x instanceof Date)
	{
		return x.getTime() === y.getTime();
	}

	if (!('ctor' in x))
	{
		for (var key in x)
		{
			if (!eqHelp(x[key], y[key], depth + 1, stack))
			{
				return false;
			}
		}
		return true;
	}

	// convert Dicts and Sets to lists
	if (x.ctor === 'RBNode_elm_builtin' || x.ctor === 'RBEmpty_elm_builtin')
	{
		x = _elm_lang$core$Dict$toList(x);
		y = _elm_lang$core$Dict$toList(y);
	}
	if (x.ctor === 'Set_elm_builtin')
	{
		x = _elm_lang$core$Set$toList(x);
		y = _elm_lang$core$Set$toList(y);
	}

	// check if lists are equal without recursion
	if (x.ctor === '::')
	{
		var a = x;
		var b = y;
		while (a.ctor === '::' && b.ctor === '::')
		{
			if (!eqHelp(a._0, b._0, depth + 1, stack))
			{
				return false;
			}
			a = a._1;
			b = b._1;
		}
		return a.ctor === b.ctor;
	}

	// check if Arrays are equal
	if (x.ctor === '_Array')
	{
		var xs = _elm_lang$core$Native_Array.toJSArray(x);
		var ys = _elm_lang$core$Native_Array.toJSArray(y);
		if (xs.length !== ys.length)
		{
			return false;
		}
		for (var i = 0; i < xs.length; i++)
		{
			if (!eqHelp(xs[i], ys[i], depth + 1, stack))
			{
				return false;
			}
		}
		return true;
	}

	if (!eqHelp(x.ctor, y.ctor, depth + 1, stack))
	{
		return false;
	}

	for (var key in x)
	{
		if (!eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

var LT = -1, EQ = 0, GT = 1;

function cmp(x, y)
{
	if (typeof x !== 'object')
	{
		return x === y ? EQ : x < y ? LT : GT;
	}

	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? EQ : a < b ? LT : GT;
	}

	if (x.ctor === '::' || x.ctor === '[]')
	{
		while (x.ctor === '::' && y.ctor === '::')
		{
			var ord = cmp(x._0, y._0);
			if (ord !== EQ)
			{
				return ord;
			}
			x = x._1;
			y = y._1;
		}
		return x.ctor === y.ctor ? EQ : x.ctor === '[]' ? LT : GT;
	}

	if (x.ctor.slice(0, 6) === '_Tuple')
	{
		var ord;
		var n = x.ctor.slice(6) - 0;
		var err = 'cannot compare tuples with more than 6 elements.';
		if (n === 0) return EQ;
		if (n >= 1) { ord = cmp(x._0, y._0); if (ord !== EQ) return ord;
		if (n >= 2) { ord = cmp(x._1, y._1); if (ord !== EQ) return ord;
		if (n >= 3) { ord = cmp(x._2, y._2); if (ord !== EQ) return ord;
		if (n >= 4) { ord = cmp(x._3, y._3); if (ord !== EQ) return ord;
		if (n >= 5) { ord = cmp(x._4, y._4); if (ord !== EQ) return ord;
		if (n >= 6) { ord = cmp(x._5, y._5); if (ord !== EQ) return ord;
		if (n >= 7) throw new Error('Comparison error: ' + err); } } } } } }
		return EQ;
	}

	throw new Error(
		'Comparison error: comparison is only defined on ints, '
		+ 'floats, times, chars, strings, lists of comparable values, '
		+ 'and tuples of comparable values.'
	);
}


// COMMON VALUES

var Tuple0 = {
	ctor: '_Tuple0'
};

function Tuple2(x, y)
{
	return {
		ctor: '_Tuple2',
		_0: x,
		_1: y
	};
}

function chr(c)
{
	return new String(c);
}


// GUID

var count = 0;
function guid(_)
{
	return count++;
}


// RECORDS

function update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


//// LIST STUFF ////

var Nil = { ctor: '[]' };

function Cons(hd, tl)
{
	return {
		ctor: '::',
		_0: hd,
		_1: tl
	};
}

function append(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (xs.ctor === '[]')
	{
		return ys;
	}
	var root = Cons(xs._0, Nil);
	var curr = root;
	xs = xs._1;
	while (xs.ctor !== '[]')
	{
		curr._1 = Cons(xs._0, Nil);
		xs = xs._1;
		curr = curr._1;
	}
	curr._1 = ys;
	return root;
}


// CRASHES

function crash(moduleName, region)
{
	return function(message) {
		throw new Error(
			'Ran into a `Debug.crash` in module `' + moduleName + '` ' + regionToString(region) + '\n'
			+ 'The message provided by the code author is:\n\n    '
			+ message
		);
	};
}

function crashCase(moduleName, region, value)
{
	return function(message) {
		throw new Error(
			'Ran into a `Debug.crash` in module `' + moduleName + '`\n\n'
			+ 'This was caused by the `case` expression ' + regionToString(region) + '.\n'
			+ 'One of the branches ended with a crash and the following value got through:\n\n    ' + toString(value) + '\n\n'
			+ 'The message provided by the code author is:\n\n    '
			+ message
		);
	};
}

function regionToString(region)
{
	if (region.start.line == region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'between lines ' + region.start.line + ' and ' + region.end.line;
}


// TO STRING

function toString(v)
{
	var type = typeof v;
	if (type === 'function')
	{
		return '<function>';
	}

	if (type === 'boolean')
	{
		return v ? 'True' : 'False';
	}

	if (type === 'number')
	{
		return v + '';
	}

	if (v instanceof String)
	{
		return '\'' + addSlashes(v, true) + '\'';
	}

	if (type === 'string')
	{
		return '"' + addSlashes(v, false) + '"';
	}

	if (v === null)
	{
		return 'null';
	}

	if (type === 'object' && 'ctor' in v)
	{
		var ctorStarter = v.ctor.substring(0, 5);

		if (ctorStarter === '_Tupl')
		{
			var output = [];
			for (var k in v)
			{
				if (k === 'ctor') continue;
				output.push(toString(v[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (ctorStarter === '_Task')
		{
			return '<task>'
		}

		if (v.ctor === '_Array')
		{
			var list = _elm_lang$core$Array$toList(v);
			return 'Array.fromList ' + toString(list);
		}

		if (v.ctor === '<decoder>')
		{
			return '<decoder>';
		}

		if (v.ctor === '_Process')
		{
			return '<process:' + v.id + '>';
		}

		if (v.ctor === '::')
		{
			var output = '[' + toString(v._0);
			v = v._1;
			while (v.ctor === '::')
			{
				output += ',' + toString(v._0);
				v = v._1;
			}
			return output + ']';
		}

		if (v.ctor === '[]')
		{
			return '[]';
		}

		if (v.ctor === 'Set_elm_builtin')
		{
			return 'Set.fromList ' + toString(_elm_lang$core$Set$toList(v));
		}

		if (v.ctor === 'RBNode_elm_builtin' || v.ctor === 'RBEmpty_elm_builtin')
		{
			return 'Dict.fromList ' + toString(_elm_lang$core$Dict$toList(v));
		}

		var output = '';
		for (var i in v)
		{
			if (i === 'ctor') continue;
			var str = toString(v[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return v.ctor + output;
	}

	if (type === 'object')
	{
		if (v instanceof Date)
		{
			return '<' + v.toString() + '>';
		}

		if (v.elm_web_socket)
		{
			return '<websocket>';
		}

		var output = [];
		for (var k in v)
		{
			output.push(k + ' = ' + toString(v[k]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return '<internal structure>';
}

function addSlashes(str, isChar)
{
	var s = str.replace(/\\/g, '\\\\')
			  .replace(/\n/g, '\\n')
			  .replace(/\t/g, '\\t')
			  .replace(/\r/g, '\\r')
			  .replace(/\v/g, '\\v')
			  .replace(/\0/g, '\\0');
	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}


return {
	eq: eq,
	cmp: cmp,
	Tuple0: Tuple0,
	Tuple2: Tuple2,
	chr: chr,
	update: update,
	guid: guid,

	append: F2(append),

	crash: crash,
	crashCase: crashCase,

	toString: toString
};

}();
var _elm_lang$core$Basics$never = function (_p0) {
	never:
	while (true) {
		var _p1 = _p0;
		var _v1 = _p1._0;
		_p0 = _v1;
		continue never;
	}
};
var _elm_lang$core$Basics$uncurry = F2(
	function (f, _p2) {
		var _p3 = _p2;
		return A2(f, _p3._0, _p3._1);
	});
var _elm_lang$core$Basics$curry = F3(
	function (f, a, b) {
		return f(
			{ctor: '_Tuple2', _0: a, _1: b});
	});
var _elm_lang$core$Basics$flip = F3(
	function (f, b, a) {
		return A2(f, a, b);
	});
var _elm_lang$core$Basics$always = F2(
	function (a, _p4) {
		return a;
	});
var _elm_lang$core$Basics$identity = function (x) {
	return x;
};
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<|'] = F2(
	function (f, x) {
		return f(x);
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['|>'] = F2(
	function (x, f) {
		return f(x);
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>>'] = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<<'] = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['++'] = _elm_lang$core$Native_Utils.append;
var _elm_lang$core$Basics$toString = _elm_lang$core$Native_Utils.toString;
var _elm_lang$core$Basics$isInfinite = _elm_lang$core$Native_Basics.isInfinite;
var _elm_lang$core$Basics$isNaN = _elm_lang$core$Native_Basics.isNaN;
var _elm_lang$core$Basics$toFloat = _elm_lang$core$Native_Basics.toFloat;
var _elm_lang$core$Basics$ceiling = _elm_lang$core$Native_Basics.ceiling;
var _elm_lang$core$Basics$floor = _elm_lang$core$Native_Basics.floor;
var _elm_lang$core$Basics$truncate = _elm_lang$core$Native_Basics.truncate;
var _elm_lang$core$Basics$round = _elm_lang$core$Native_Basics.round;
var _elm_lang$core$Basics$not = _elm_lang$core$Native_Basics.not;
var _elm_lang$core$Basics$xor = _elm_lang$core$Native_Basics.xor;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['||'] = _elm_lang$core$Native_Basics.or;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['&&'] = _elm_lang$core$Native_Basics.and;
var _elm_lang$core$Basics$max = _elm_lang$core$Native_Basics.max;
var _elm_lang$core$Basics$min = _elm_lang$core$Native_Basics.min;
var _elm_lang$core$Basics$compare = _elm_lang$core$Native_Basics.compare;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>='] = _elm_lang$core$Native_Basics.ge;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<='] = _elm_lang$core$Native_Basics.le;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>'] = _elm_lang$core$Native_Basics.gt;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<'] = _elm_lang$core$Native_Basics.lt;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['/='] = _elm_lang$core$Native_Basics.neq;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['=='] = _elm_lang$core$Native_Basics.eq;
var _elm_lang$core$Basics$e = _elm_lang$core$Native_Basics.e;
var _elm_lang$core$Basics$pi = _elm_lang$core$Native_Basics.pi;
var _elm_lang$core$Basics$clamp = _elm_lang$core$Native_Basics.clamp;
var _elm_lang$core$Basics$logBase = _elm_lang$core$Native_Basics.logBase;
var _elm_lang$core$Basics$abs = _elm_lang$core$Native_Basics.abs;
var _elm_lang$core$Basics$negate = _elm_lang$core$Native_Basics.negate;
var _elm_lang$core$Basics$sqrt = _elm_lang$core$Native_Basics.sqrt;
var _elm_lang$core$Basics$atan2 = _elm_lang$core$Native_Basics.atan2;
var _elm_lang$core$Basics$atan = _elm_lang$core$Native_Basics.atan;
var _elm_lang$core$Basics$asin = _elm_lang$core$Native_Basics.asin;
var _elm_lang$core$Basics$acos = _elm_lang$core$Native_Basics.acos;
var _elm_lang$core$Basics$tan = _elm_lang$core$Native_Basics.tan;
var _elm_lang$core$Basics$sin = _elm_lang$core$Native_Basics.sin;
var _elm_lang$core$Basics$cos = _elm_lang$core$Native_Basics.cos;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['^'] = _elm_lang$core$Native_Basics.exp;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['%'] = _elm_lang$core$Native_Basics.mod;
var _elm_lang$core$Basics$rem = _elm_lang$core$Native_Basics.rem;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['//'] = _elm_lang$core$Native_Basics.div;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['/'] = _elm_lang$core$Native_Basics.floatDiv;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['*'] = _elm_lang$core$Native_Basics.mul;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['-'] = _elm_lang$core$Native_Basics.sub;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['+'] = _elm_lang$core$Native_Basics.add;
var _elm_lang$core$Basics$toPolar = _elm_lang$core$Native_Basics.toPolar;
var _elm_lang$core$Basics$fromPolar = _elm_lang$core$Native_Basics.fromPolar;
var _elm_lang$core$Basics$turns = _elm_lang$core$Native_Basics.turns;
var _elm_lang$core$Basics$degrees = _elm_lang$core$Native_Basics.degrees;
var _elm_lang$core$Basics$radians = function (t) {
	return t;
};
var _elm_lang$core$Basics$GT = {ctor: 'GT'};
var _elm_lang$core$Basics$EQ = {ctor: 'EQ'};
var _elm_lang$core$Basics$LT = {ctor: 'LT'};
var _elm_lang$core$Basics$JustOneMore = function (a) {
	return {ctor: 'JustOneMore', _0: a};
};

var _elm_lang$core$Maybe$withDefault = F2(
	function ($default, maybe) {
		var _p0 = maybe;
		if (_p0.ctor === 'Just') {
			return _p0._0;
		} else {
			return $default;
		}
	});
var _elm_lang$core$Maybe$Nothing = {ctor: 'Nothing'};
var _elm_lang$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		var _p1 = maybeValue;
		if (_p1.ctor === 'Just') {
			return callback(_p1._0);
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$Just = function (a) {
	return {ctor: 'Just', _0: a};
};
var _elm_lang$core$Maybe$map = F2(
	function (f, maybe) {
		var _p2 = maybe;
		if (_p2.ctor === 'Just') {
			return _elm_lang$core$Maybe$Just(
				f(_p2._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		var _p3 = {ctor: '_Tuple2', _0: ma, _1: mb};
		if (((_p3.ctor === '_Tuple2') && (_p3._0.ctor === 'Just')) && (_p3._1.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A2(func, _p3._0._0, _p3._1._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map3 = F4(
	function (func, ma, mb, mc) {
		var _p4 = {ctor: '_Tuple3', _0: ma, _1: mb, _2: mc};
		if ((((_p4.ctor === '_Tuple3') && (_p4._0.ctor === 'Just')) && (_p4._1.ctor === 'Just')) && (_p4._2.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A3(func, _p4._0._0, _p4._1._0, _p4._2._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map4 = F5(
	function (func, ma, mb, mc, md) {
		var _p5 = {ctor: '_Tuple4', _0: ma, _1: mb, _2: mc, _3: md};
		if (((((_p5.ctor === '_Tuple4') && (_p5._0.ctor === 'Just')) && (_p5._1.ctor === 'Just')) && (_p5._2.ctor === 'Just')) && (_p5._3.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A4(func, _p5._0._0, _p5._1._0, _p5._2._0, _p5._3._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map5 = F6(
	function (func, ma, mb, mc, md, me) {
		var _p6 = {ctor: '_Tuple5', _0: ma, _1: mb, _2: mc, _3: md, _4: me};
		if ((((((_p6.ctor === '_Tuple5') && (_p6._0.ctor === 'Just')) && (_p6._1.ctor === 'Just')) && (_p6._2.ctor === 'Just')) && (_p6._3.ctor === 'Just')) && (_p6._4.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A5(func, _p6._0._0, _p6._1._0, _p6._2._0, _p6._3._0, _p6._4._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});

//import Native.Utils //

var _elm_lang$core$Native_List = function() {

var Nil = { ctor: '[]' };

function Cons(hd, tl)
{
	return { ctor: '::', _0: hd, _1: tl };
}

function fromArray(arr)
{
	var out = Nil;
	for (var i = arr.length; i--; )
	{
		out = Cons(arr[i], out);
	}
	return out;
}

function toArray(xs)
{
	var out = [];
	while (xs.ctor !== '[]')
	{
		out.push(xs._0);
		xs = xs._1;
	}
	return out;
}

function foldr(f, b, xs)
{
	var arr = toArray(xs);
	var acc = b;
	for (var i = arr.length; i--; )
	{
		acc = A2(f, arr[i], acc);
	}
	return acc;
}

function map2(f, xs, ys)
{
	var arr = [];
	while (xs.ctor !== '[]' && ys.ctor !== '[]')
	{
		arr.push(A2(f, xs._0, ys._0));
		xs = xs._1;
		ys = ys._1;
	}
	return fromArray(arr);
}

function map3(f, xs, ys, zs)
{
	var arr = [];
	while (xs.ctor !== '[]' && ys.ctor !== '[]' && zs.ctor !== '[]')
	{
		arr.push(A3(f, xs._0, ys._0, zs._0));
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function map4(f, ws, xs, ys, zs)
{
	var arr = [];
	while (   ws.ctor !== '[]'
		   && xs.ctor !== '[]'
		   && ys.ctor !== '[]'
		   && zs.ctor !== '[]')
	{
		arr.push(A4(f, ws._0, xs._0, ys._0, zs._0));
		ws = ws._1;
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function map5(f, vs, ws, xs, ys, zs)
{
	var arr = [];
	while (   vs.ctor !== '[]'
		   && ws.ctor !== '[]'
		   && xs.ctor !== '[]'
		   && ys.ctor !== '[]'
		   && zs.ctor !== '[]')
	{
		arr.push(A5(f, vs._0, ws._0, xs._0, ys._0, zs._0));
		vs = vs._1;
		ws = ws._1;
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function sortBy(f, xs)
{
	return fromArray(toArray(xs).sort(function(a, b) {
		return _elm_lang$core$Native_Utils.cmp(f(a), f(b));
	}));
}

function sortWith(f, xs)
{
	return fromArray(toArray(xs).sort(function(a, b) {
		var ord = f(a)(b).ctor;
		return ord === 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
	}));
}

return {
	Nil: Nil,
	Cons: Cons,
	cons: F2(Cons),
	toArray: toArray,
	fromArray: fromArray,

	foldr: F3(foldr),

	map2: F3(map2),
	map3: F4(map3),
	map4: F5(map4),
	map5: F6(map5),
	sortBy: F2(sortBy),
	sortWith: F2(sortWith)
};

}();
var _elm_lang$core$List$sortWith = _elm_lang$core$Native_List.sortWith;
var _elm_lang$core$List$sortBy = _elm_lang$core$Native_List.sortBy;
var _elm_lang$core$List$sort = function (xs) {
	return A2(_elm_lang$core$List$sortBy, _elm_lang$core$Basics$identity, xs);
};
var _elm_lang$core$List$singleton = function (value) {
	return {
		ctor: '::',
		_0: value,
		_1: {ctor: '[]'}
	};
};
var _elm_lang$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return list;
			} else {
				var _p0 = list;
				if (_p0.ctor === '[]') {
					return list;
				} else {
					var _v1 = n - 1,
						_v2 = _p0._1;
					n = _v1;
					list = _v2;
					continue drop;
				}
			}
		}
	});
var _elm_lang$core$List$map5 = _elm_lang$core$Native_List.map5;
var _elm_lang$core$List$map4 = _elm_lang$core$Native_List.map4;
var _elm_lang$core$List$map3 = _elm_lang$core$Native_List.map3;
var _elm_lang$core$List$map2 = _elm_lang$core$Native_List.map2;
var _elm_lang$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			var _p1 = list;
			if (_p1.ctor === '[]') {
				return false;
			} else {
				if (isOkay(_p1._0)) {
					return true;
				} else {
					var _v4 = isOkay,
						_v5 = _p1._1;
					isOkay = _v4;
					list = _v5;
					continue any;
				}
			}
		}
	});
var _elm_lang$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			_elm_lang$core$List$any,
			function (_p2) {
				return !isOkay(_p2);
			},
			list);
	});
var _elm_lang$core$List$foldr = _elm_lang$core$Native_List.foldr;
var _elm_lang$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			var _p3 = list;
			if (_p3.ctor === '[]') {
				return acc;
			} else {
				var _v7 = func,
					_v8 = A2(func, _p3._0, acc),
					_v9 = _p3._1;
				func = _v7;
				acc = _v8;
				list = _v9;
				continue foldl;
			}
		}
	});
var _elm_lang$core$List$length = function (xs) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (_p4, i) {
				return i + 1;
			}),
		0,
		xs);
};
var _elm_lang$core$List$sum = function (numbers) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return x + y;
			}),
		0,
		numbers);
};
var _elm_lang$core$List$product = function (numbers) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return x * y;
			}),
		1,
		numbers);
};
var _elm_lang$core$List$maximum = function (list) {
	var _p5 = list;
	if (_p5.ctor === '::') {
		return _elm_lang$core$Maybe$Just(
			A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$max, _p5._0, _p5._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$minimum = function (list) {
	var _p6 = list;
	if (_p6.ctor === '::') {
		return _elm_lang$core$Maybe$Just(
			A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$min, _p6._0, _p6._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$member = F2(
	function (x, xs) {
		return A2(
			_elm_lang$core$List$any,
			function (a) {
				return _elm_lang$core$Native_Utils.eq(a, x);
			},
			xs);
	});
var _elm_lang$core$List$isEmpty = function (xs) {
	var _p7 = xs;
	if (_p7.ctor === '[]') {
		return true;
	} else {
		return false;
	}
};
var _elm_lang$core$List$tail = function (list) {
	var _p8 = list;
	if (_p8.ctor === '::') {
		return _elm_lang$core$Maybe$Just(_p8._1);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$head = function (list) {
	var _p9 = list;
	if (_p9.ctor === '::') {
		return _elm_lang$core$Maybe$Just(_p9._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List_ops = _elm_lang$core$List_ops || {};
_elm_lang$core$List_ops['::'] = _elm_lang$core$Native_List.cons;
var _elm_lang$core$List$map = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldr,
			F2(
				function (x, acc) {
					return {
						ctor: '::',
						_0: f(x),
						_1: acc
					};
				}),
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$filter = F2(
	function (pred, xs) {
		var conditionalCons = F2(
			function (front, back) {
				return pred(front) ? {ctor: '::', _0: front, _1: back} : back;
			});
		return A3(
			_elm_lang$core$List$foldr,
			conditionalCons,
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _p10 = f(mx);
		if (_p10.ctor === 'Just') {
			return {ctor: '::', _0: _p10._0, _1: xs};
		} else {
			return xs;
		}
	});
var _elm_lang$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldr,
			_elm_lang$core$List$maybeCons(f),
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$reverse = function (list) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return {ctor: '::', _0: x, _1: y};
			}),
		{ctor: '[]'},
		list);
};
var _elm_lang$core$List$scanl = F3(
	function (f, b, xs) {
		var scan1 = F2(
			function (x, accAcc) {
				var _p11 = accAcc;
				if (_p11.ctor === '::') {
					return {
						ctor: '::',
						_0: A2(f, x, _p11._0),
						_1: accAcc
					};
				} else {
					return {ctor: '[]'};
				}
			});
		return _elm_lang$core$List$reverse(
			A3(
				_elm_lang$core$List$foldl,
				scan1,
				{
					ctor: '::',
					_0: b,
					_1: {ctor: '[]'}
				},
				xs));
	});
var _elm_lang$core$List$append = F2(
	function (xs, ys) {
		var _p12 = ys;
		if (_p12.ctor === '[]') {
			return xs;
		} else {
			return A3(
				_elm_lang$core$List$foldr,
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					}),
				ys,
				xs);
		}
	});
var _elm_lang$core$List$concat = function (lists) {
	return A3(
		_elm_lang$core$List$foldr,
		_elm_lang$core$List$append,
		{ctor: '[]'},
		lists);
};
var _elm_lang$core$List$concatMap = F2(
	function (f, list) {
		return _elm_lang$core$List$concat(
			A2(_elm_lang$core$List$map, f, list));
	});
var _elm_lang$core$List$partition = F2(
	function (pred, list) {
		var step = F2(
			function (x, _p13) {
				var _p14 = _p13;
				var _p16 = _p14._0;
				var _p15 = _p14._1;
				return pred(x) ? {
					ctor: '_Tuple2',
					_0: {ctor: '::', _0: x, _1: _p16},
					_1: _p15
				} : {
					ctor: '_Tuple2',
					_0: _p16,
					_1: {ctor: '::', _0: x, _1: _p15}
				};
			});
		return A3(
			_elm_lang$core$List$foldr,
			step,
			{
				ctor: '_Tuple2',
				_0: {ctor: '[]'},
				_1: {ctor: '[]'}
			},
			list);
	});
var _elm_lang$core$List$unzip = function (pairs) {
	var step = F2(
		function (_p18, _p17) {
			var _p19 = _p18;
			var _p20 = _p17;
			return {
				ctor: '_Tuple2',
				_0: {ctor: '::', _0: _p19._0, _1: _p20._0},
				_1: {ctor: '::', _0: _p19._1, _1: _p20._1}
			};
		});
	return A3(
		_elm_lang$core$List$foldr,
		step,
		{
			ctor: '_Tuple2',
			_0: {ctor: '[]'},
			_1: {ctor: '[]'}
		},
		pairs);
};
var _elm_lang$core$List$intersperse = F2(
	function (sep, xs) {
		var _p21 = xs;
		if (_p21.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var step = F2(
				function (x, rest) {
					return {
						ctor: '::',
						_0: sep,
						_1: {ctor: '::', _0: x, _1: rest}
					};
				});
			var spersed = A3(
				_elm_lang$core$List$foldr,
				step,
				{ctor: '[]'},
				_p21._1);
			return {ctor: '::', _0: _p21._0, _1: spersed};
		}
	});
var _elm_lang$core$List$takeReverse = F3(
	function (n, list, taken) {
		takeReverse:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return taken;
			} else {
				var _p22 = list;
				if (_p22.ctor === '[]') {
					return taken;
				} else {
					var _v23 = n - 1,
						_v24 = _p22._1,
						_v25 = {ctor: '::', _0: _p22._0, _1: taken};
					n = _v23;
					list = _v24;
					taken = _v25;
					continue takeReverse;
				}
			}
		}
	});
var _elm_lang$core$List$takeTailRec = F2(
	function (n, list) {
		return _elm_lang$core$List$reverse(
			A3(
				_elm_lang$core$List$takeReverse,
				n,
				list,
				{ctor: '[]'}));
	});
var _elm_lang$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
			return {ctor: '[]'};
		} else {
			var _p23 = {ctor: '_Tuple2', _0: n, _1: list};
			_v26_5:
			do {
				_v26_1:
				do {
					if (_p23.ctor === '_Tuple2') {
						if (_p23._1.ctor === '[]') {
							return list;
						} else {
							if (_p23._1._1.ctor === '::') {
								switch (_p23._0) {
									case 1:
										break _v26_1;
									case 2:
										return {
											ctor: '::',
											_0: _p23._1._0,
											_1: {
												ctor: '::',
												_0: _p23._1._1._0,
												_1: {ctor: '[]'}
											}
										};
									case 3:
										if (_p23._1._1._1.ctor === '::') {
											return {
												ctor: '::',
												_0: _p23._1._0,
												_1: {
													ctor: '::',
													_0: _p23._1._1._0,
													_1: {
														ctor: '::',
														_0: _p23._1._1._1._0,
														_1: {ctor: '[]'}
													}
												}
											};
										} else {
											break _v26_5;
										}
									default:
										if ((_p23._1._1._1.ctor === '::') && (_p23._1._1._1._1.ctor === '::')) {
											var _p28 = _p23._1._1._1._0;
											var _p27 = _p23._1._1._0;
											var _p26 = _p23._1._0;
											var _p25 = _p23._1._1._1._1._0;
											var _p24 = _p23._1._1._1._1._1;
											return (_elm_lang$core$Native_Utils.cmp(ctr, 1000) > 0) ? {
												ctor: '::',
												_0: _p26,
												_1: {
													ctor: '::',
													_0: _p27,
													_1: {
														ctor: '::',
														_0: _p28,
														_1: {
															ctor: '::',
															_0: _p25,
															_1: A2(_elm_lang$core$List$takeTailRec, n - 4, _p24)
														}
													}
												}
											} : {
												ctor: '::',
												_0: _p26,
												_1: {
													ctor: '::',
													_0: _p27,
													_1: {
														ctor: '::',
														_0: _p28,
														_1: {
															ctor: '::',
															_0: _p25,
															_1: A3(_elm_lang$core$List$takeFast, ctr + 1, n - 4, _p24)
														}
													}
												}
											};
										} else {
											break _v26_5;
										}
								}
							} else {
								if (_p23._0 === 1) {
									break _v26_1;
								} else {
									break _v26_5;
								}
							}
						}
					} else {
						break _v26_5;
					}
				} while(false);
				return {
					ctor: '::',
					_0: _p23._1._0,
					_1: {ctor: '[]'}
				};
			} while(false);
			return list;
		}
	});
var _elm_lang$core$List$take = F2(
	function (n, list) {
		return A3(_elm_lang$core$List$takeFast, 0, n, list);
	});
var _elm_lang$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return result;
			} else {
				var _v27 = {ctor: '::', _0: value, _1: result},
					_v28 = n - 1,
					_v29 = value;
				result = _v27;
				n = _v28;
				value = _v29;
				continue repeatHelp;
			}
		}
	});
var _elm_lang$core$List$repeat = F2(
	function (n, value) {
		return A3(
			_elm_lang$core$List$repeatHelp,
			{ctor: '[]'},
			n,
			value);
	});
var _elm_lang$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(lo, hi) < 1) {
				var _v30 = lo,
					_v31 = hi - 1,
					_v32 = {ctor: '::', _0: hi, _1: list};
				lo = _v30;
				hi = _v31;
				list = _v32;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var _elm_lang$core$List$range = F2(
	function (lo, hi) {
		return A3(
			_elm_lang$core$List$rangeHelp,
			lo,
			hi,
			{ctor: '[]'});
	});
var _elm_lang$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$map2,
			f,
			A2(
				_elm_lang$core$List$range,
				0,
				_elm_lang$core$List$length(xs) - 1),
			xs);
	});

//import Native.Utils //

var _elm_lang$core$Native_Debug = function() {

function log(tag, value)
{
	var msg = tag + ': ' + _elm_lang$core$Native_Utils.toString(value);
	var process = process || {};
	if (process.stdout)
	{
		process.stdout.write(msg);
	}
	else
	{
		console.log(msg);
	}
	return value;
}

function crash(message)
{
	throw new Error(message);
}

return {
	crash: crash,
	log: F2(log)
};

}();
//import Maybe, Native.List, Native.Utils, Result //

var _elm_lang$core$Native_String = function() {

function isEmpty(str)
{
	return str.length === 0;
}
function cons(chr, str)
{
	return chr + str;
}
function uncons(str)
{
	var hd = str[0];
	if (hd)
	{
		return _elm_lang$core$Maybe$Just(_elm_lang$core$Native_Utils.Tuple2(_elm_lang$core$Native_Utils.chr(hd), str.slice(1)));
	}
	return _elm_lang$core$Maybe$Nothing;
}
function append(a, b)
{
	return a + b;
}
function concat(strs)
{
	return _elm_lang$core$Native_List.toArray(strs).join('');
}
function length(str)
{
	return str.length;
}
function map(f, str)
{
	var out = str.split('');
	for (var i = out.length; i--; )
	{
		out[i] = f(_elm_lang$core$Native_Utils.chr(out[i]));
	}
	return out.join('');
}
function filter(pred, str)
{
	return str.split('').map(_elm_lang$core$Native_Utils.chr).filter(pred).join('');
}
function reverse(str)
{
	return str.split('').reverse().join('');
}
function foldl(f, b, str)
{
	var len = str.length;
	for (var i = 0; i < len; ++i)
	{
		b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
	}
	return b;
}
function foldr(f, b, str)
{
	for (var i = str.length; i--; )
	{
		b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
	}
	return b;
}
function split(sep, str)
{
	return _elm_lang$core$Native_List.fromArray(str.split(sep));
}
function join(sep, strs)
{
	return _elm_lang$core$Native_List.toArray(strs).join(sep);
}
function repeat(n, str)
{
	var result = '';
	while (n > 0)
	{
		if (n & 1)
		{
			result += str;
		}
		n >>= 1, str += str;
	}
	return result;
}
function slice(start, end, str)
{
	return str.slice(start, end);
}
function left(n, str)
{
	return n < 1 ? '' : str.slice(0, n);
}
function right(n, str)
{
	return n < 1 ? '' : str.slice(-n);
}
function dropLeft(n, str)
{
	return n < 1 ? str : str.slice(n);
}
function dropRight(n, str)
{
	return n < 1 ? str : str.slice(0, -n);
}
function pad(n, chr, str)
{
	var half = (n - str.length) / 2;
	return repeat(Math.ceil(half), chr) + str + repeat(half | 0, chr);
}
function padRight(n, chr, str)
{
	return str + repeat(n - str.length, chr);
}
function padLeft(n, chr, str)
{
	return repeat(n - str.length, chr) + str;
}

function trim(str)
{
	return str.trim();
}
function trimLeft(str)
{
	return str.replace(/^\s+/, '');
}
function trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function words(str)
{
	return _elm_lang$core$Native_List.fromArray(str.trim().split(/\s+/g));
}
function lines(str)
{
	return _elm_lang$core$Native_List.fromArray(str.split(/\r\n|\r|\n/g));
}

function toUpper(str)
{
	return str.toUpperCase();
}
function toLower(str)
{
	return str.toLowerCase();
}

function any(pred, str)
{
	for (var i = str.length; i--; )
	{
		if (pred(_elm_lang$core$Native_Utils.chr(str[i])))
		{
			return true;
		}
	}
	return false;
}
function all(pred, str)
{
	for (var i = str.length; i--; )
	{
		if (!pred(_elm_lang$core$Native_Utils.chr(str[i])))
		{
			return false;
		}
	}
	return true;
}

function contains(sub, str)
{
	return str.indexOf(sub) > -1;
}
function startsWith(sub, str)
{
	return str.indexOf(sub) === 0;
}
function endsWith(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
}
function indexes(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _elm_lang$core$Native_List.Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _elm_lang$core$Native_List.fromArray(is);
}


function toInt(s)
{
	var len = s.length;

	// if empty
	if (len === 0)
	{
		return intErr(s);
	}

	// if hex
	var c = s[0];
	if (c === '0' && s[1] === 'x')
	{
		for (var i = 2; i < len; ++i)
		{
			var c = s[i];
			if (('0' <= c && c <= '9') || ('A' <= c && c <= 'F') || ('a' <= c && c <= 'f'))
			{
				continue;
			}
			return intErr(s);
		}
		return _elm_lang$core$Result$Ok(parseInt(s, 16));
	}

	// is decimal
	if (c > '9' || (c < '0' && c !== '-' && c !== '+'))
	{
		return intErr(s);
	}
	for (var i = 1; i < len; ++i)
	{
		var c = s[i];
		if (c < '0' || '9' < c)
		{
			return intErr(s);
		}
	}

	return _elm_lang$core$Result$Ok(parseInt(s, 10));
}

function intErr(s)
{
	return _elm_lang$core$Result$Err("could not convert string '" + s + "' to an Int");
}


function toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return floatErr(s);
	}
	var n = +s;
	// faster isNaN check
	return n === n ? _elm_lang$core$Result$Ok(n) : floatErr(s);
}

function floatErr(s)
{
	return _elm_lang$core$Result$Err("could not convert string '" + s + "' to a Float");
}


function toList(str)
{
	return _elm_lang$core$Native_List.fromArray(str.split('').map(_elm_lang$core$Native_Utils.chr));
}
function fromList(chars)
{
	return _elm_lang$core$Native_List.toArray(chars).join('');
}

return {
	isEmpty: isEmpty,
	cons: F2(cons),
	uncons: uncons,
	append: F2(append),
	concat: concat,
	length: length,
	map: F2(map),
	filter: F2(filter),
	reverse: reverse,
	foldl: F3(foldl),
	foldr: F3(foldr),

	split: F2(split),
	join: F2(join),
	repeat: F2(repeat),

	slice: F3(slice),
	left: F2(left),
	right: F2(right),
	dropLeft: F2(dropLeft),
	dropRight: F2(dropRight),

	pad: F3(pad),
	padLeft: F3(padLeft),
	padRight: F3(padRight),

	trim: trim,
	trimLeft: trimLeft,
	trimRight: trimRight,

	words: words,
	lines: lines,

	toUpper: toUpper,
	toLower: toLower,

	any: F2(any),
	all: F2(all),

	contains: F2(contains),
	startsWith: F2(startsWith),
	endsWith: F2(endsWith),
	indexes: F2(indexes),

	toInt: toInt,
	toFloat: toFloat,
	toList: toList,
	fromList: fromList
};

}();

//import Native.Utils //

var _elm_lang$core$Native_Char = function() {

return {
	fromCode: function(c) { return _elm_lang$core$Native_Utils.chr(String.fromCharCode(c)); },
	toCode: function(c) { return c.charCodeAt(0); },
	toUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toUpperCase()); },
	toLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLowerCase()); },
	toLocaleUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleUpperCase()); },
	toLocaleLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleLowerCase()); }
};

}();
var _elm_lang$core$Char$fromCode = _elm_lang$core$Native_Char.fromCode;
var _elm_lang$core$Char$toCode = _elm_lang$core$Native_Char.toCode;
var _elm_lang$core$Char$toLocaleLower = _elm_lang$core$Native_Char.toLocaleLower;
var _elm_lang$core$Char$toLocaleUpper = _elm_lang$core$Native_Char.toLocaleUpper;
var _elm_lang$core$Char$toLower = _elm_lang$core$Native_Char.toLower;
var _elm_lang$core$Char$toUpper = _elm_lang$core$Native_Char.toUpper;
var _elm_lang$core$Char$isBetween = F3(
	function (low, high, $char) {
		var code = _elm_lang$core$Char$toCode($char);
		return (_elm_lang$core$Native_Utils.cmp(
			code,
			_elm_lang$core$Char$toCode(low)) > -1) && (_elm_lang$core$Native_Utils.cmp(
			code,
			_elm_lang$core$Char$toCode(high)) < 1);
	});
var _elm_lang$core$Char$isUpper = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('A'),
	_elm_lang$core$Native_Utils.chr('Z'));
var _elm_lang$core$Char$isLower = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('a'),
	_elm_lang$core$Native_Utils.chr('z'));
var _elm_lang$core$Char$isDigit = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('0'),
	_elm_lang$core$Native_Utils.chr('9'));
var _elm_lang$core$Char$isOctDigit = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('0'),
	_elm_lang$core$Native_Utils.chr('7'));
var _elm_lang$core$Char$isHexDigit = function ($char) {
	return _elm_lang$core$Char$isDigit($char) || (A3(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('a'),
		_elm_lang$core$Native_Utils.chr('f'),
		$char) || A3(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('A'),
		_elm_lang$core$Native_Utils.chr('F'),
		$char));
};

var _elm_lang$core$Result$toMaybe = function (result) {
	var _p0 = result;
	if (_p0.ctor === 'Ok') {
		return _elm_lang$core$Maybe$Just(_p0._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$Result$withDefault = F2(
	function (def, result) {
		var _p1 = result;
		if (_p1.ctor === 'Ok') {
			return _p1._0;
		} else {
			return def;
		}
	});
var _elm_lang$core$Result$Err = function (a) {
	return {ctor: 'Err', _0: a};
};
var _elm_lang$core$Result$andThen = F2(
	function (callback, result) {
		var _p2 = result;
		if (_p2.ctor === 'Ok') {
			return callback(_p2._0);
		} else {
			return _elm_lang$core$Result$Err(_p2._0);
		}
	});
var _elm_lang$core$Result$Ok = function (a) {
	return {ctor: 'Ok', _0: a};
};
var _elm_lang$core$Result$map = F2(
	function (func, ra) {
		var _p3 = ra;
		if (_p3.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(
				func(_p3._0));
		} else {
			return _elm_lang$core$Result$Err(_p3._0);
		}
	});
var _elm_lang$core$Result$map2 = F3(
	function (func, ra, rb) {
		var _p4 = {ctor: '_Tuple2', _0: ra, _1: rb};
		if (_p4._0.ctor === 'Ok') {
			if (_p4._1.ctor === 'Ok') {
				return _elm_lang$core$Result$Ok(
					A2(func, _p4._0._0, _p4._1._0));
			} else {
				return _elm_lang$core$Result$Err(_p4._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p4._0._0);
		}
	});
var _elm_lang$core$Result$map3 = F4(
	function (func, ra, rb, rc) {
		var _p5 = {ctor: '_Tuple3', _0: ra, _1: rb, _2: rc};
		if (_p5._0.ctor === 'Ok') {
			if (_p5._1.ctor === 'Ok') {
				if (_p5._2.ctor === 'Ok') {
					return _elm_lang$core$Result$Ok(
						A3(func, _p5._0._0, _p5._1._0, _p5._2._0));
				} else {
					return _elm_lang$core$Result$Err(_p5._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p5._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p5._0._0);
		}
	});
var _elm_lang$core$Result$map4 = F5(
	function (func, ra, rb, rc, rd) {
		var _p6 = {ctor: '_Tuple4', _0: ra, _1: rb, _2: rc, _3: rd};
		if (_p6._0.ctor === 'Ok') {
			if (_p6._1.ctor === 'Ok') {
				if (_p6._2.ctor === 'Ok') {
					if (_p6._3.ctor === 'Ok') {
						return _elm_lang$core$Result$Ok(
							A4(func, _p6._0._0, _p6._1._0, _p6._2._0, _p6._3._0));
					} else {
						return _elm_lang$core$Result$Err(_p6._3._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p6._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p6._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p6._0._0);
		}
	});
var _elm_lang$core$Result$map5 = F6(
	function (func, ra, rb, rc, rd, re) {
		var _p7 = {ctor: '_Tuple5', _0: ra, _1: rb, _2: rc, _3: rd, _4: re};
		if (_p7._0.ctor === 'Ok') {
			if (_p7._1.ctor === 'Ok') {
				if (_p7._2.ctor === 'Ok') {
					if (_p7._3.ctor === 'Ok') {
						if (_p7._4.ctor === 'Ok') {
							return _elm_lang$core$Result$Ok(
								A5(func, _p7._0._0, _p7._1._0, _p7._2._0, _p7._3._0, _p7._4._0));
						} else {
							return _elm_lang$core$Result$Err(_p7._4._0);
						}
					} else {
						return _elm_lang$core$Result$Err(_p7._3._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p7._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p7._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p7._0._0);
		}
	});
var _elm_lang$core$Result$mapError = F2(
	function (f, result) {
		var _p8 = result;
		if (_p8.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(_p8._0);
		} else {
			return _elm_lang$core$Result$Err(
				f(_p8._0));
		}
	});
var _elm_lang$core$Result$fromMaybe = F2(
	function (err, maybe) {
		var _p9 = maybe;
		if (_p9.ctor === 'Just') {
			return _elm_lang$core$Result$Ok(_p9._0);
		} else {
			return _elm_lang$core$Result$Err(err);
		}
	});

var _elm_lang$core$String$fromList = _elm_lang$core$Native_String.fromList;
var _elm_lang$core$String$toList = _elm_lang$core$Native_String.toList;
var _elm_lang$core$String$toFloat = _elm_lang$core$Native_String.toFloat;
var _elm_lang$core$String$toInt = _elm_lang$core$Native_String.toInt;
var _elm_lang$core$String$indices = _elm_lang$core$Native_String.indexes;
var _elm_lang$core$String$indexes = _elm_lang$core$Native_String.indexes;
var _elm_lang$core$String$endsWith = _elm_lang$core$Native_String.endsWith;
var _elm_lang$core$String$startsWith = _elm_lang$core$Native_String.startsWith;
var _elm_lang$core$String$contains = _elm_lang$core$Native_String.contains;
var _elm_lang$core$String$all = _elm_lang$core$Native_String.all;
var _elm_lang$core$String$any = _elm_lang$core$Native_String.any;
var _elm_lang$core$String$toLower = _elm_lang$core$Native_String.toLower;
var _elm_lang$core$String$toUpper = _elm_lang$core$Native_String.toUpper;
var _elm_lang$core$String$lines = _elm_lang$core$Native_String.lines;
var _elm_lang$core$String$words = _elm_lang$core$Native_String.words;
var _elm_lang$core$String$trimRight = _elm_lang$core$Native_String.trimRight;
var _elm_lang$core$String$trimLeft = _elm_lang$core$Native_String.trimLeft;
var _elm_lang$core$String$trim = _elm_lang$core$Native_String.trim;
var _elm_lang$core$String$padRight = _elm_lang$core$Native_String.padRight;
var _elm_lang$core$String$padLeft = _elm_lang$core$Native_String.padLeft;
var _elm_lang$core$String$pad = _elm_lang$core$Native_String.pad;
var _elm_lang$core$String$dropRight = _elm_lang$core$Native_String.dropRight;
var _elm_lang$core$String$dropLeft = _elm_lang$core$Native_String.dropLeft;
var _elm_lang$core$String$right = _elm_lang$core$Native_String.right;
var _elm_lang$core$String$left = _elm_lang$core$Native_String.left;
var _elm_lang$core$String$slice = _elm_lang$core$Native_String.slice;
var _elm_lang$core$String$repeat = _elm_lang$core$Native_String.repeat;
var _elm_lang$core$String$join = _elm_lang$core$Native_String.join;
var _elm_lang$core$String$split = _elm_lang$core$Native_String.split;
var _elm_lang$core$String$foldr = _elm_lang$core$Native_String.foldr;
var _elm_lang$core$String$foldl = _elm_lang$core$Native_String.foldl;
var _elm_lang$core$String$reverse = _elm_lang$core$Native_String.reverse;
var _elm_lang$core$String$filter = _elm_lang$core$Native_String.filter;
var _elm_lang$core$String$map = _elm_lang$core$Native_String.map;
var _elm_lang$core$String$length = _elm_lang$core$Native_String.length;
var _elm_lang$core$String$concat = _elm_lang$core$Native_String.concat;
var _elm_lang$core$String$append = _elm_lang$core$Native_String.append;
var _elm_lang$core$String$uncons = _elm_lang$core$Native_String.uncons;
var _elm_lang$core$String$cons = _elm_lang$core$Native_String.cons;
var _elm_lang$core$String$fromChar = function ($char) {
	return A2(_elm_lang$core$String$cons, $char, '');
};
var _elm_lang$core$String$isEmpty = _elm_lang$core$Native_String.isEmpty;

var _elm_lang$core$Dict$foldr = F3(
	function (f, acc, t) {
		foldr:
		while (true) {
			var _p0 = t;
			if (_p0.ctor === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var _v1 = f,
					_v2 = A3(
					f,
					_p0._1,
					_p0._2,
					A3(_elm_lang$core$Dict$foldr, f, acc, _p0._4)),
					_v3 = _p0._3;
				f = _v1;
				acc = _v2;
				t = _v3;
				continue foldr;
			}
		}
	});
var _elm_lang$core$Dict$keys = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return {ctor: '::', _0: key, _1: keyList};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$values = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return {ctor: '::', _0: value, _1: valueList};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$toList = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: key, _1: value},
					_1: list
				};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$foldl = F3(
	function (f, acc, dict) {
		foldl:
		while (true) {
			var _p1 = dict;
			if (_p1.ctor === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var _v5 = f,
					_v6 = A3(
					f,
					_p1._1,
					_p1._2,
					A3(_elm_lang$core$Dict$foldl, f, acc, _p1._3)),
					_v7 = _p1._4;
				f = _v5;
				acc = _v6;
				dict = _v7;
				continue foldl;
			}
		}
	});
var _elm_lang$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _p2) {
				stepState:
				while (true) {
					var _p3 = _p2;
					var _p9 = _p3._1;
					var _p8 = _p3._0;
					var _p4 = _p8;
					if (_p4.ctor === '[]') {
						return {
							ctor: '_Tuple2',
							_0: _p8,
							_1: A3(rightStep, rKey, rValue, _p9)
						};
					} else {
						var _p7 = _p4._1;
						var _p6 = _p4._0._1;
						var _p5 = _p4._0._0;
						if (_elm_lang$core$Native_Utils.cmp(_p5, rKey) < 0) {
							var _v10 = rKey,
								_v11 = rValue,
								_v12 = {
								ctor: '_Tuple2',
								_0: _p7,
								_1: A3(leftStep, _p5, _p6, _p9)
							};
							rKey = _v10;
							rValue = _v11;
							_p2 = _v12;
							continue stepState;
						} else {
							if (_elm_lang$core$Native_Utils.cmp(_p5, rKey) > 0) {
								return {
									ctor: '_Tuple2',
									_0: _p8,
									_1: A3(rightStep, rKey, rValue, _p9)
								};
							} else {
								return {
									ctor: '_Tuple2',
									_0: _p7,
									_1: A4(bothStep, _p5, _p6, rValue, _p9)
								};
							}
						}
					}
				}
			});
		var _p10 = A3(
			_elm_lang$core$Dict$foldl,
			stepState,
			{
				ctor: '_Tuple2',
				_0: _elm_lang$core$Dict$toList(leftDict),
				_1: initialResult
			},
			rightDict);
		var leftovers = _p10._0;
		var intermediateResult = _p10._1;
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (_p11, result) {
					var _p12 = _p11;
					return A3(leftStep, _p12._0, _p12._1, result);
				}),
			intermediateResult,
			leftovers);
	});
var _elm_lang$core$Dict$reportRemBug = F4(
	function (msg, c, lgot, rgot) {
		return _elm_lang$core$Native_Debug.crash(
			_elm_lang$core$String$concat(
				{
					ctor: '::',
					_0: 'Internal red-black tree invariant violated, expected ',
					_1: {
						ctor: '::',
						_0: msg,
						_1: {
							ctor: '::',
							_0: ' and got ',
							_1: {
								ctor: '::',
								_0: _elm_lang$core$Basics$toString(c),
								_1: {
									ctor: '::',
									_0: '/',
									_1: {
										ctor: '::',
										_0: lgot,
										_1: {
											ctor: '::',
											_0: '/',
											_1: {
												ctor: '::',
												_0: rgot,
												_1: {
													ctor: '::',
													_0: '\nPlease report this bug to <https://github.com/elm-lang/core/issues>',
													_1: {ctor: '[]'}
												}
											}
										}
									}
								}
							}
						}
					}
				}));
	});
var _elm_lang$core$Dict$isBBlack = function (dict) {
	var _p13 = dict;
	_v14_2:
	do {
		if (_p13.ctor === 'RBNode_elm_builtin') {
			if (_p13._0.ctor === 'BBlack') {
				return true;
			} else {
				break _v14_2;
			}
		} else {
			if (_p13._0.ctor === 'LBBlack') {
				return true;
			} else {
				break _v14_2;
			}
		}
	} while(false);
	return false;
};
var _elm_lang$core$Dict$sizeHelp = F2(
	function (n, dict) {
		sizeHelp:
		while (true) {
			var _p14 = dict;
			if (_p14.ctor === 'RBEmpty_elm_builtin') {
				return n;
			} else {
				var _v16 = A2(_elm_lang$core$Dict$sizeHelp, n + 1, _p14._4),
					_v17 = _p14._3;
				n = _v16;
				dict = _v17;
				continue sizeHelp;
			}
		}
	});
var _elm_lang$core$Dict$size = function (dict) {
	return A2(_elm_lang$core$Dict$sizeHelp, 0, dict);
};
var _elm_lang$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			var _p15 = dict;
			if (_p15.ctor === 'RBEmpty_elm_builtin') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				var _p16 = A2(_elm_lang$core$Basics$compare, targetKey, _p15._1);
				switch (_p16.ctor) {
					case 'LT':
						var _v20 = targetKey,
							_v21 = _p15._3;
						targetKey = _v20;
						dict = _v21;
						continue get;
					case 'EQ':
						return _elm_lang$core$Maybe$Just(_p15._2);
					default:
						var _v22 = targetKey,
							_v23 = _p15._4;
						targetKey = _v22;
						dict = _v23;
						continue get;
				}
			}
		}
	});
var _elm_lang$core$Dict$member = F2(
	function (key, dict) {
		var _p17 = A2(_elm_lang$core$Dict$get, key, dict);
		if (_p17.ctor === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var _elm_lang$core$Dict$maxWithDefault = F3(
	function (k, v, r) {
		maxWithDefault:
		while (true) {
			var _p18 = r;
			if (_p18.ctor === 'RBEmpty_elm_builtin') {
				return {ctor: '_Tuple2', _0: k, _1: v};
			} else {
				var _v26 = _p18._1,
					_v27 = _p18._2,
					_v28 = _p18._4;
				k = _v26;
				v = _v27;
				r = _v28;
				continue maxWithDefault;
			}
		}
	});
var _elm_lang$core$Dict$NBlack = {ctor: 'NBlack'};
var _elm_lang$core$Dict$BBlack = {ctor: 'BBlack'};
var _elm_lang$core$Dict$Black = {ctor: 'Black'};
var _elm_lang$core$Dict$blackish = function (t) {
	var _p19 = t;
	if (_p19.ctor === 'RBNode_elm_builtin') {
		var _p20 = _p19._0;
		return _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$Black) || _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$BBlack);
	} else {
		return true;
	}
};
var _elm_lang$core$Dict$Red = {ctor: 'Red'};
var _elm_lang$core$Dict$moreBlack = function (color) {
	var _p21 = color;
	switch (_p21.ctor) {
		case 'Black':
			return _elm_lang$core$Dict$BBlack;
		case 'Red':
			return _elm_lang$core$Dict$Black;
		case 'NBlack':
			return _elm_lang$core$Dict$Red;
		default:
			return _elm_lang$core$Native_Debug.crash('Can\'t make a double black node more black!');
	}
};
var _elm_lang$core$Dict$lessBlack = function (color) {
	var _p22 = color;
	switch (_p22.ctor) {
		case 'BBlack':
			return _elm_lang$core$Dict$Black;
		case 'Black':
			return _elm_lang$core$Dict$Red;
		case 'Red':
			return _elm_lang$core$Dict$NBlack;
		default:
			return _elm_lang$core$Native_Debug.crash('Can\'t make a negative black node less black!');
	}
};
var _elm_lang$core$Dict$LBBlack = {ctor: 'LBBlack'};
var _elm_lang$core$Dict$LBlack = {ctor: 'LBlack'};
var _elm_lang$core$Dict$RBEmpty_elm_builtin = function (a) {
	return {ctor: 'RBEmpty_elm_builtin', _0: a};
};
var _elm_lang$core$Dict$empty = _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
var _elm_lang$core$Dict$isEmpty = function (dict) {
	return _elm_lang$core$Native_Utils.eq(dict, _elm_lang$core$Dict$empty);
};
var _elm_lang$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {ctor: 'RBNode_elm_builtin', _0: a, _1: b, _2: c, _3: d, _4: e};
	});
var _elm_lang$core$Dict$ensureBlackRoot = function (dict) {
	var _p23 = dict;
	if ((_p23.ctor === 'RBNode_elm_builtin') && (_p23._0.ctor === 'Red')) {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p23._1, _p23._2, _p23._3, _p23._4);
	} else {
		return dict;
	}
};
var _elm_lang$core$Dict$lessBlackTree = function (dict) {
	var _p24 = dict;
	if (_p24.ctor === 'RBNode_elm_builtin') {
		return A5(
			_elm_lang$core$Dict$RBNode_elm_builtin,
			_elm_lang$core$Dict$lessBlack(_p24._0),
			_p24._1,
			_p24._2,
			_p24._3,
			_p24._4);
	} else {
		return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
	}
};
var _elm_lang$core$Dict$balancedTree = function (col) {
	return function (xk) {
		return function (xv) {
			return function (yk) {
				return function (yv) {
					return function (zk) {
						return function (zv) {
							return function (a) {
								return function (b) {
									return function (c) {
										return function (d) {
											return A5(
												_elm_lang$core$Dict$RBNode_elm_builtin,
												_elm_lang$core$Dict$lessBlack(col),
												yk,
												yv,
												A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, xk, xv, a, b),
												A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, zk, zv, c, d));
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _elm_lang$core$Dict$blacken = function (t) {
	var _p25 = t;
	if (_p25.ctor === 'RBEmpty_elm_builtin') {
		return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
	} else {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p25._1, _p25._2, _p25._3, _p25._4);
	}
};
var _elm_lang$core$Dict$redden = function (t) {
	var _p26 = t;
	if (_p26.ctor === 'RBEmpty_elm_builtin') {
		return _elm_lang$core$Native_Debug.crash('can\'t make a Leaf red');
	} else {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, _p26._1, _p26._2, _p26._3, _p26._4);
	}
};
var _elm_lang$core$Dict$balanceHelp = function (tree) {
	var _p27 = tree;
	_v36_6:
	do {
		_v36_5:
		do {
			_v36_4:
			do {
				_v36_3:
				do {
					_v36_2:
					do {
						_v36_1:
						do {
							_v36_0:
							do {
								if (_p27.ctor === 'RBNode_elm_builtin') {
									if (_p27._3.ctor === 'RBNode_elm_builtin') {
										if (_p27._4.ctor === 'RBNode_elm_builtin') {
											switch (_p27._3._0.ctor) {
												case 'Red':
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																		break _v36_2;
																	} else {
																		if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																			break _v36_3;
																		} else {
																			break _v36_6;
																		}
																	}
																}
															}
														case 'NBlack':
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																		break _v36_4;
																	} else {
																		break _v36_6;
																	}
																}
															}
														default:
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	break _v36_6;
																}
															}
													}
												case 'NBlack':
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																break _v36_2;
															} else {
																if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																	break _v36_3;
																} else {
																	if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																		break _v36_5;
																	} else {
																		break _v36_6;
																	}
																}
															}
														case 'NBlack':
															if (_p27._0.ctor === 'BBlack') {
																if ((((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																	break _v36_4;
																} else {
																	if ((((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																		break _v36_5;
																	} else {
																		break _v36_6;
																	}
																}
															} else {
																break _v36_6;
															}
														default:
															if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																break _v36_5;
															} else {
																break _v36_6;
															}
													}
												default:
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																break _v36_2;
															} else {
																if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																	break _v36_3;
																} else {
																	break _v36_6;
																}
															}
														case 'NBlack':
															if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																break _v36_4;
															} else {
																break _v36_6;
															}
														default:
															break _v36_6;
													}
											}
										} else {
											switch (_p27._3._0.ctor) {
												case 'Red':
													if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
														break _v36_0;
													} else {
														if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
															break _v36_1;
														} else {
															break _v36_6;
														}
													}
												case 'NBlack':
													if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
														break _v36_5;
													} else {
														break _v36_6;
													}
												default:
													break _v36_6;
											}
										}
									} else {
										if (_p27._4.ctor === 'RBNode_elm_builtin') {
											switch (_p27._4._0.ctor) {
												case 'Red':
													if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
														break _v36_2;
													} else {
														if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
															break _v36_3;
														} else {
															break _v36_6;
														}
													}
												case 'NBlack':
													if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
														break _v36_4;
													} else {
														break _v36_6;
													}
												default:
													break _v36_6;
											}
										} else {
											break _v36_6;
										}
									}
								} else {
									break _v36_6;
								}
							} while(false);
							return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._3._1)(_p27._3._3._2)(_p27._3._1)(_p27._3._2)(_p27._1)(_p27._2)(_p27._3._3._3)(_p27._3._3._4)(_p27._3._4)(_p27._4);
						} while(false);
						return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._1)(_p27._3._2)(_p27._3._4._1)(_p27._3._4._2)(_p27._1)(_p27._2)(_p27._3._3)(_p27._3._4._3)(_p27._3._4._4)(_p27._4);
					} while(false);
					return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._3._1)(_p27._4._3._2)(_p27._4._1)(_p27._4._2)(_p27._3)(_p27._4._3._3)(_p27._4._3._4)(_p27._4._4);
				} while(false);
				return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._1)(_p27._4._2)(_p27._4._4._1)(_p27._4._4._2)(_p27._3)(_p27._4._3)(_p27._4._4._3)(_p27._4._4._4);
			} while(false);
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_elm_lang$core$Dict$Black,
				_p27._4._3._1,
				_p27._4._3._2,
				A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3, _p27._4._3._3),
				A5(
					_elm_lang$core$Dict$balance,
					_elm_lang$core$Dict$Black,
					_p27._4._1,
					_p27._4._2,
					_p27._4._3._4,
					_elm_lang$core$Dict$redden(_p27._4._4)));
		} while(false);
		return A5(
			_elm_lang$core$Dict$RBNode_elm_builtin,
			_elm_lang$core$Dict$Black,
			_p27._3._4._1,
			_p27._3._4._2,
			A5(
				_elm_lang$core$Dict$balance,
				_elm_lang$core$Dict$Black,
				_p27._3._1,
				_p27._3._2,
				_elm_lang$core$Dict$redden(_p27._3._3),
				_p27._3._4._3),
			A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3._4._4, _p27._4));
	} while(false);
	return tree;
};
var _elm_lang$core$Dict$balance = F5(
	function (c, k, v, l, r) {
		var tree = A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
		return _elm_lang$core$Dict$blackish(tree) ? _elm_lang$core$Dict$balanceHelp(tree) : tree;
	});
var _elm_lang$core$Dict$bubble = F5(
	function (c, k, v, l, r) {
		return (_elm_lang$core$Dict$isBBlack(l) || _elm_lang$core$Dict$isBBlack(r)) ? A5(
			_elm_lang$core$Dict$balance,
			_elm_lang$core$Dict$moreBlack(c),
			k,
			v,
			_elm_lang$core$Dict$lessBlackTree(l),
			_elm_lang$core$Dict$lessBlackTree(r)) : A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
	});
var _elm_lang$core$Dict$removeMax = F5(
	function (c, k, v, l, r) {
		var _p28 = r;
		if (_p28.ctor === 'RBEmpty_elm_builtin') {
			return A3(_elm_lang$core$Dict$rem, c, l, r);
		} else {
			return A5(
				_elm_lang$core$Dict$bubble,
				c,
				k,
				v,
				l,
				A5(_elm_lang$core$Dict$removeMax, _p28._0, _p28._1, _p28._2, _p28._3, _p28._4));
		}
	});
var _elm_lang$core$Dict$rem = F3(
	function (color, left, right) {
		var _p29 = {ctor: '_Tuple2', _0: left, _1: right};
		if (_p29._0.ctor === 'RBEmpty_elm_builtin') {
			if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
				var _p30 = color;
				switch (_p30.ctor) {
					case 'Red':
						return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
					case 'Black':
						return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBBlack);
					default:
						return _elm_lang$core$Native_Debug.crash('cannot have bblack or nblack nodes at this point');
				}
			} else {
				var _p33 = _p29._1._0;
				var _p32 = _p29._0._0;
				var _p31 = {ctor: '_Tuple3', _0: color, _1: _p32, _2: _p33};
				if ((((_p31.ctor === '_Tuple3') && (_p31._0.ctor === 'Black')) && (_p31._1.ctor === 'LBlack')) && (_p31._2.ctor === 'Red')) {
					return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._1._1, _p29._1._2, _p29._1._3, _p29._1._4);
				} else {
					return A4(
						_elm_lang$core$Dict$reportRemBug,
						'Black/LBlack/Red',
						color,
						_elm_lang$core$Basics$toString(_p32),
						_elm_lang$core$Basics$toString(_p33));
				}
			}
		} else {
			if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
				var _p36 = _p29._1._0;
				var _p35 = _p29._0._0;
				var _p34 = {ctor: '_Tuple3', _0: color, _1: _p35, _2: _p36};
				if ((((_p34.ctor === '_Tuple3') && (_p34._0.ctor === 'Black')) && (_p34._1.ctor === 'Red')) && (_p34._2.ctor === 'LBlack')) {
					return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._0._1, _p29._0._2, _p29._0._3, _p29._0._4);
				} else {
					return A4(
						_elm_lang$core$Dict$reportRemBug,
						'Black/Red/LBlack',
						color,
						_elm_lang$core$Basics$toString(_p35),
						_elm_lang$core$Basics$toString(_p36));
				}
			} else {
				var _p40 = _p29._0._2;
				var _p39 = _p29._0._4;
				var _p38 = _p29._0._1;
				var newLeft = A5(_elm_lang$core$Dict$removeMax, _p29._0._0, _p38, _p40, _p29._0._3, _p39);
				var _p37 = A3(_elm_lang$core$Dict$maxWithDefault, _p38, _p40, _p39);
				var k = _p37._0;
				var v = _p37._1;
				return A5(_elm_lang$core$Dict$bubble, color, k, v, newLeft, right);
			}
		}
	});
var _elm_lang$core$Dict$map = F2(
	function (f, dict) {
		var _p41 = dict;
		if (_p41.ctor === 'RBEmpty_elm_builtin') {
			return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
		} else {
			var _p42 = _p41._1;
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_p41._0,
				_p42,
				A2(f, _p42, _p41._2),
				A2(_elm_lang$core$Dict$map, f, _p41._3),
				A2(_elm_lang$core$Dict$map, f, _p41._4));
		}
	});
var _elm_lang$core$Dict$Same = {ctor: 'Same'};
var _elm_lang$core$Dict$Remove = {ctor: 'Remove'};
var _elm_lang$core$Dict$Insert = {ctor: 'Insert'};
var _elm_lang$core$Dict$update = F3(
	function (k, alter, dict) {
		var up = function (dict) {
			var _p43 = dict;
			if (_p43.ctor === 'RBEmpty_elm_builtin') {
				var _p44 = alter(_elm_lang$core$Maybe$Nothing);
				if (_p44.ctor === 'Nothing') {
					return {ctor: '_Tuple2', _0: _elm_lang$core$Dict$Same, _1: _elm_lang$core$Dict$empty};
				} else {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Dict$Insert,
						_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, k, _p44._0, _elm_lang$core$Dict$empty, _elm_lang$core$Dict$empty)
					};
				}
			} else {
				var _p55 = _p43._2;
				var _p54 = _p43._4;
				var _p53 = _p43._3;
				var _p52 = _p43._1;
				var _p51 = _p43._0;
				var _p45 = A2(_elm_lang$core$Basics$compare, k, _p52);
				switch (_p45.ctor) {
					case 'EQ':
						var _p46 = alter(
							_elm_lang$core$Maybe$Just(_p55));
						if (_p46.ctor === 'Nothing') {
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Dict$Remove,
								_1: A3(_elm_lang$core$Dict$rem, _p51, _p53, _p54)
							};
						} else {
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Dict$Same,
								_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p46._0, _p53, _p54)
							};
						}
					case 'LT':
						var _p47 = up(_p53);
						var flag = _p47._0;
						var newLeft = _p47._1;
						var _p48 = flag;
						switch (_p48.ctor) {
							case 'Same':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Same,
									_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, newLeft, _p54)
								};
							case 'Insert':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Insert,
									_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, newLeft, _p54)
								};
							default:
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Remove,
									_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, newLeft, _p54)
								};
						}
					default:
						var _p49 = up(_p54);
						var flag = _p49._0;
						var newRight = _p49._1;
						var _p50 = flag;
						switch (_p50.ctor) {
							case 'Same':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Same,
									_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, _p53, newRight)
								};
							case 'Insert':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Insert,
									_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, _p53, newRight)
								};
							default:
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Remove,
									_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, _p53, newRight)
								};
						}
				}
			}
		};
		var _p56 = up(dict);
		var flag = _p56._0;
		var updatedDict = _p56._1;
		var _p57 = flag;
		switch (_p57.ctor) {
			case 'Same':
				return updatedDict;
			case 'Insert':
				return _elm_lang$core$Dict$ensureBlackRoot(updatedDict);
			default:
				return _elm_lang$core$Dict$blacken(updatedDict);
		}
	});
var _elm_lang$core$Dict$insert = F3(
	function (key, value, dict) {
		return A3(
			_elm_lang$core$Dict$update,
			key,
			_elm_lang$core$Basics$always(
				_elm_lang$core$Maybe$Just(value)),
			dict);
	});
var _elm_lang$core$Dict$singleton = F2(
	function (key, value) {
		return A3(_elm_lang$core$Dict$insert, key, value, _elm_lang$core$Dict$empty);
	});
var _elm_lang$core$Dict$union = F2(
	function (t1, t2) {
		return A3(_elm_lang$core$Dict$foldl, _elm_lang$core$Dict$insert, t2, t1);
	});
var _elm_lang$core$Dict$filter = F2(
	function (predicate, dictionary) {
		var add = F3(
			function (key, value, dict) {
				return A2(predicate, key, value) ? A3(_elm_lang$core$Dict$insert, key, value, dict) : dict;
			});
		return A3(_elm_lang$core$Dict$foldl, add, _elm_lang$core$Dict$empty, dictionary);
	});
var _elm_lang$core$Dict$intersect = F2(
	function (t1, t2) {
		return A2(
			_elm_lang$core$Dict$filter,
			F2(
				function (k, _p58) {
					return A2(_elm_lang$core$Dict$member, k, t2);
				}),
			t1);
	});
var _elm_lang$core$Dict$partition = F2(
	function (predicate, dict) {
		var add = F3(
			function (key, value, _p59) {
				var _p60 = _p59;
				var _p62 = _p60._1;
				var _p61 = _p60._0;
				return A2(predicate, key, value) ? {
					ctor: '_Tuple2',
					_0: A3(_elm_lang$core$Dict$insert, key, value, _p61),
					_1: _p62
				} : {
					ctor: '_Tuple2',
					_0: _p61,
					_1: A3(_elm_lang$core$Dict$insert, key, value, _p62)
				};
			});
		return A3(
			_elm_lang$core$Dict$foldl,
			add,
			{ctor: '_Tuple2', _0: _elm_lang$core$Dict$empty, _1: _elm_lang$core$Dict$empty},
			dict);
	});
var _elm_lang$core$Dict$fromList = function (assocs) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (_p63, dict) {
				var _p64 = _p63;
				return A3(_elm_lang$core$Dict$insert, _p64._0, _p64._1, dict);
			}),
		_elm_lang$core$Dict$empty,
		assocs);
};
var _elm_lang$core$Dict$remove = F2(
	function (key, dict) {
		return A3(
			_elm_lang$core$Dict$update,
			key,
			_elm_lang$core$Basics$always(_elm_lang$core$Maybe$Nothing),
			dict);
	});
var _elm_lang$core$Dict$diff = F2(
	function (t1, t2) {
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, v, t) {
					return A2(_elm_lang$core$Dict$remove, k, t);
				}),
			t1,
			t2);
	});

var _elm_lang$core$Set$foldr = F3(
	function (f, b, _p0) {
		var _p1 = _p0;
		return A3(
			_elm_lang$core$Dict$foldr,
			F3(
				function (k, _p2, b) {
					return A2(f, k, b);
				}),
			b,
			_p1._0);
	});
var _elm_lang$core$Set$foldl = F3(
	function (f, b, _p3) {
		var _p4 = _p3;
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, _p5, b) {
					return A2(f, k, b);
				}),
			b,
			_p4._0);
	});
var _elm_lang$core$Set$toList = function (_p6) {
	var _p7 = _p6;
	return _elm_lang$core$Dict$keys(_p7._0);
};
var _elm_lang$core$Set$size = function (_p8) {
	var _p9 = _p8;
	return _elm_lang$core$Dict$size(_p9._0);
};
var _elm_lang$core$Set$member = F2(
	function (k, _p10) {
		var _p11 = _p10;
		return A2(_elm_lang$core$Dict$member, k, _p11._0);
	});
var _elm_lang$core$Set$isEmpty = function (_p12) {
	var _p13 = _p12;
	return _elm_lang$core$Dict$isEmpty(_p13._0);
};
var _elm_lang$core$Set$Set_elm_builtin = function (a) {
	return {ctor: 'Set_elm_builtin', _0: a};
};
var _elm_lang$core$Set$empty = _elm_lang$core$Set$Set_elm_builtin(_elm_lang$core$Dict$empty);
var _elm_lang$core$Set$singleton = function (k) {
	return _elm_lang$core$Set$Set_elm_builtin(
		A2(
			_elm_lang$core$Dict$singleton,
			k,
			{ctor: '_Tuple0'}));
};
var _elm_lang$core$Set$insert = F2(
	function (k, _p14) {
		var _p15 = _p14;
		return _elm_lang$core$Set$Set_elm_builtin(
			A3(
				_elm_lang$core$Dict$insert,
				k,
				{ctor: '_Tuple0'},
				_p15._0));
	});
var _elm_lang$core$Set$fromList = function (xs) {
	return A3(_elm_lang$core$List$foldl, _elm_lang$core$Set$insert, _elm_lang$core$Set$empty, xs);
};
var _elm_lang$core$Set$map = F2(
	function (f, s) {
		return _elm_lang$core$Set$fromList(
			A2(
				_elm_lang$core$List$map,
				f,
				_elm_lang$core$Set$toList(s)));
	});
var _elm_lang$core$Set$remove = F2(
	function (k, _p16) {
		var _p17 = _p16;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$remove, k, _p17._0));
	});
var _elm_lang$core$Set$union = F2(
	function (_p19, _p18) {
		var _p20 = _p19;
		var _p21 = _p18;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$union, _p20._0, _p21._0));
	});
var _elm_lang$core$Set$intersect = F2(
	function (_p23, _p22) {
		var _p24 = _p23;
		var _p25 = _p22;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$intersect, _p24._0, _p25._0));
	});
var _elm_lang$core$Set$diff = F2(
	function (_p27, _p26) {
		var _p28 = _p27;
		var _p29 = _p26;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$diff, _p28._0, _p29._0));
	});
var _elm_lang$core$Set$filter = F2(
	function (p, _p30) {
		var _p31 = _p30;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(
				_elm_lang$core$Dict$filter,
				F2(
					function (k, _p32) {
						return p(k);
					}),
				_p31._0));
	});
var _elm_lang$core$Set$partition = F2(
	function (p, _p33) {
		var _p34 = _p33;
		var _p35 = A2(
			_elm_lang$core$Dict$partition,
			F2(
				function (k, _p36) {
					return p(k);
				}),
			_p34._0);
		var p1 = _p35._0;
		var p2 = _p35._1;
		return {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Set$Set_elm_builtin(p1),
			_1: _elm_lang$core$Set$Set_elm_builtin(p2)
		};
	});

var _elm_lang$core$Debug$crash = _elm_lang$core$Native_Debug.crash;
var _elm_lang$core$Debug$log = _elm_lang$core$Native_Debug.log;

var _elm_lang$core$Tuple$mapSecond = F2(
	function (func, _p0) {
		var _p1 = _p0;
		return {
			ctor: '_Tuple2',
			_0: _p1._0,
			_1: func(_p1._1)
		};
	});
var _elm_lang$core$Tuple$mapFirst = F2(
	function (func, _p2) {
		var _p3 = _p2;
		return {
			ctor: '_Tuple2',
			_0: func(_p3._0),
			_1: _p3._1
		};
	});
var _elm_lang$core$Tuple$second = function (_p4) {
	var _p5 = _p4;
	return _p5._1;
};
var _elm_lang$core$Tuple$first = function (_p6) {
	var _p7 = _p6;
	return _p7._0;
};

//import //

var _elm_lang$core$Native_Platform = function() {


// PROGRAMS

function program(impl)
{
	return function(flagDecoder)
	{
		return function(object, moduleName)
		{
			object['worker'] = function worker(flags)
			{
				if (typeof flags !== 'undefined')
				{
					throw new Error(
						'The `' + moduleName + '` module does not need flags.\n'
						+ 'Call ' + moduleName + '.worker() with no arguments and you should be all set!'
					);
				}

				return initialize(
					impl.init,
					impl.update,
					impl.subscriptions,
					renderer
				);
			};
		};
	};
}

function programWithFlags(impl)
{
	return function(flagDecoder)
	{
		return function(object, moduleName)
		{
			object['worker'] = function worker(flags)
			{
				if (typeof flagDecoder === 'undefined')
				{
					throw new Error(
						'Are you trying to sneak a Never value into Elm? Trickster!\n'
						+ 'It looks like ' + moduleName + '.main is defined with `programWithFlags` but has type `Program Never`.\n'
						+ 'Use `program` instead if you do not want flags.'
					);
				}

				var result = A2(_elm_lang$core$Native_Json.run, flagDecoder, flags);
				if (result.ctor === 'Err')
				{
					throw new Error(
						moduleName + '.worker(...) was called with an unexpected argument.\n'
						+ 'I tried to convert it to an Elm value, but ran into this problem:\n\n'
						+ result._0
					);
				}

				return initialize(
					impl.init(result._0),
					impl.update,
					impl.subscriptions,
					renderer
				);
			};
		};
	};
}

function renderer(enqueue, _)
{
	return function(_) {};
}


// HTML TO PROGRAM

function htmlToProgram(vnode)
{
	var emptyBag = batch(_elm_lang$core$Native_List.Nil);
	var noChange = _elm_lang$core$Native_Utils.Tuple2(
		_elm_lang$core$Native_Utils.Tuple0,
		emptyBag
	);

	return _elm_lang$virtual_dom$VirtualDom$program({
		init: noChange,
		view: function(model) { return main; },
		update: F2(function(msg, model) { return noChange; }),
		subscriptions: function (model) { return emptyBag; }
	});
}


// INITIALIZE A PROGRAM

function initialize(init, update, subscriptions, renderer)
{
	// ambient state
	var managers = {};
	var updateView;

	// init and update state in main process
	var initApp = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
		var model = init._0;
		updateView = renderer(enqueue, model);
		var cmds = init._1;
		var subs = subscriptions(model);
		dispatchEffects(managers, cmds, subs);
		callback(_elm_lang$core$Native_Scheduler.succeed(model));
	});

	function onMessage(msg, model)
	{
		return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
			var results = A2(update, msg, model);
			model = results._0;
			updateView(model);
			var cmds = results._1;
			var subs = subscriptions(model);
			dispatchEffects(managers, cmds, subs);
			callback(_elm_lang$core$Native_Scheduler.succeed(model));
		});
	}

	var mainProcess = spawnLoop(initApp, onMessage);

	function enqueue(msg)
	{
		_elm_lang$core$Native_Scheduler.rawSend(mainProcess, msg);
	}

	var ports = setupEffects(managers, enqueue);

	return ports ? { ports: ports } : {};
}


// EFFECT MANAGERS

var effectManagers = {};

function setupEffects(managers, callback)
{
	var ports;

	// setup all necessary effect managers
	for (var key in effectManagers)
	{
		var manager = effectManagers[key];

		if (manager.isForeign)
		{
			ports = ports || {};
			ports[key] = manager.tag === 'cmd'
				? setupOutgoingPort(key)
				: setupIncomingPort(key, callback);
		}

		managers[key] = makeManager(manager, callback);
	}

	return ports;
}

function makeManager(info, callback)
{
	var router = {
		main: callback,
		self: undefined
	};

	var tag = info.tag;
	var onEffects = info.onEffects;
	var onSelfMsg = info.onSelfMsg;

	function onMessage(msg, state)
	{
		if (msg.ctor === 'self')
		{
			return A3(onSelfMsg, router, msg._0, state);
		}

		var fx = msg._0;
		switch (tag)
		{
			case 'cmd':
				return A3(onEffects, router, fx.cmds, state);

			case 'sub':
				return A3(onEffects, router, fx.subs, state);

			case 'fx':
				return A4(onEffects, router, fx.cmds, fx.subs, state);
		}
	}

	var process = spawnLoop(info.init, onMessage);
	router.self = process;
	return process;
}

function sendToApp(router, msg)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		router.main(msg);
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function sendToSelf(router, msg)
{
	return A2(_elm_lang$core$Native_Scheduler.send, router.self, {
		ctor: 'self',
		_0: msg
	});
}


// HELPER for STATEFUL LOOPS

function spawnLoop(init, onMessage)
{
	var andThen = _elm_lang$core$Native_Scheduler.andThen;

	function loop(state)
	{
		var handleMsg = _elm_lang$core$Native_Scheduler.receive(function(msg) {
			return onMessage(msg, state);
		});
		return A2(andThen, loop, handleMsg);
	}

	var task = A2(andThen, loop, init);

	return _elm_lang$core$Native_Scheduler.rawSpawn(task);
}


// BAGS

function leaf(home)
{
	return function(value)
	{
		return {
			type: 'leaf',
			home: home,
			value: value
		};
	};
}

function batch(list)
{
	return {
		type: 'node',
		branches: list
	};
}

function map(tagger, bag)
{
	return {
		type: 'map',
		tagger: tagger,
		tree: bag
	}
}


// PIPE BAGS INTO EFFECT MANAGERS

function dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	gatherEffects(true, cmdBag, effectsDict, null);
	gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		var fx = home in effectsDict
			? effectsDict[home]
			: {
				cmds: _elm_lang$core$Native_List.Nil,
				subs: _elm_lang$core$Native_List.Nil
			};

		_elm_lang$core$Native_Scheduler.rawSend(managers[home], { ctor: 'fx', _0: fx });
	}
}

function gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.type)
	{
		case 'leaf':
			var home = bag.home;
			var effect = toEffect(isCmd, home, taggers, bag.value);
			effectsDict[home] = insert(isCmd, effect, effectsDict[home]);
			return;

		case 'node':
			var list = bag.branches;
			while (list.ctor !== '[]')
			{
				gatherEffects(isCmd, list._0, effectsDict, taggers);
				list = list._1;
			}
			return;

		case 'map':
			gatherEffects(isCmd, bag.tree, effectsDict, {
				tagger: bag.tagger,
				rest: taggers
			});
			return;
	}
}

function toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		var temp = taggers;
		while (temp)
		{
			x = temp.tagger(x);
			temp = temp.rest;
		}
		return x;
	}

	var map = isCmd
		? effectManagers[home].cmdMap
		: effectManagers[home].subMap;

	return A2(map, applyTaggers, value)
}

function insert(isCmd, newEffect, effects)
{
	effects = effects || {
		cmds: _elm_lang$core$Native_List.Nil,
		subs: _elm_lang$core$Native_List.Nil
	};
	if (isCmd)
	{
		effects.cmds = _elm_lang$core$Native_List.Cons(newEffect, effects.cmds);
		return effects;
	}
	effects.subs = _elm_lang$core$Native_List.Cons(newEffect, effects.subs);
	return effects;
}


// PORTS

function checkPortName(name)
{
	if (name in effectManagers)
	{
		throw new Error('There can only be one port named `' + name + '`, but your program has multiple.');
	}
}


// OUTGOING PORTS

function outgoingPort(name, converter)
{
	checkPortName(name);
	effectManagers[name] = {
		tag: 'cmd',
		cmdMap: outgoingPortMap,
		converter: converter,
		isForeign: true
	};
	return leaf(name);
}

var outgoingPortMap = F2(function cmdMap(tagger, value) {
	return value;
});

function setupOutgoingPort(name)
{
	var subs = [];
	var converter = effectManagers[name].converter;

	// CREATE MANAGER

	var init = _elm_lang$core$Native_Scheduler.succeed(null);

	function onEffects(router, cmdList, state)
	{
		while (cmdList.ctor !== '[]')
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = converter(cmdList._0);
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
			cmdList = cmdList._1;
		}
		return init;
	}

	effectManagers[name].init = init;
	effectManagers[name].onEffects = F3(onEffects);

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}


// INCOMING PORTS

function incomingPort(name, converter)
{
	checkPortName(name);
	effectManagers[name] = {
		tag: 'sub',
		subMap: incomingPortMap,
		converter: converter,
		isForeign: true
	};
	return leaf(name);
}

var incomingPortMap = F2(function subMap(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});

function setupIncomingPort(name, callback)
{
	var sentBeforeInit = [];
	var subs = _elm_lang$core$Native_List.Nil;
	var converter = effectManagers[name].converter;
	var currentOnEffects = preInitOnEffects;
	var currentSend = preInitSend;

	// CREATE MANAGER

	var init = _elm_lang$core$Native_Scheduler.succeed(null);

	function preInitOnEffects(router, subList, state)
	{
		var postInitResult = postInitOnEffects(router, subList, state);

		for(var i = 0; i < sentBeforeInit.length; i++)
		{
			postInitSend(sentBeforeInit[i]);
		}

		sentBeforeInit = null; // to release objects held in queue
		currentSend = postInitSend;
		currentOnEffects = postInitOnEffects;
		return postInitResult;
	}

	function postInitOnEffects(router, subList, state)
	{
		subs = subList;
		return init;
	}

	function onEffects(router, subList, state)
	{
		return currentOnEffects(router, subList, state);
	}

	effectManagers[name].init = init;
	effectManagers[name].onEffects = F3(onEffects);

	// PUBLIC API

	function preInitSend(value)
	{
		sentBeforeInit.push(value);
	}

	function postInitSend(value)
	{
		var temp = subs;
		while (temp.ctor !== '[]')
		{
			callback(temp._0(value));
			temp = temp._1;
		}
	}

	function send(incomingValue)
	{
		var result = A2(_elm_lang$core$Json_Decode$decodeValue, converter, incomingValue);
		if (result.ctor === 'Err')
		{
			throw new Error('Trying to send an unexpected type of value through port `' + name + '`:\n' + result._0);
		}

		currentSend(result._0);
	}

	return { send: send };
}

return {
	// routers
	sendToApp: F2(sendToApp),
	sendToSelf: F2(sendToSelf),

	// global setup
	effectManagers: effectManagers,
	outgoingPort: outgoingPort,
	incomingPort: incomingPort,

	htmlToProgram: htmlToProgram,
	program: program,
	programWithFlags: programWithFlags,
	initialize: initialize,

	// effect bags
	leaf: leaf,
	batch: batch,
	map: F2(map)
};

}();

//import Native.Utils //

var _elm_lang$core$Native_Scheduler = function() {

var MAX_STEPS = 10000;


// TASKS

function succeed(value)
{
	return {
		ctor: '_Task_succeed',
		value: value
	};
}

function fail(error)
{
	return {
		ctor: '_Task_fail',
		value: error
	};
}

function nativeBinding(callback)
{
	return {
		ctor: '_Task_nativeBinding',
		callback: callback,
		cancel: null
	};
}

function andThen(callback, task)
{
	return {
		ctor: '_Task_andThen',
		callback: callback,
		task: task
	};
}

function onError(callback, task)
{
	return {
		ctor: '_Task_onError',
		callback: callback,
		task: task
	};
}

function receive(callback)
{
	return {
		ctor: '_Task_receive',
		callback: callback
	};
}


// PROCESSES

function rawSpawn(task)
{
	var process = {
		ctor: '_Process',
		id: _elm_lang$core$Native_Utils.guid(),
		root: task,
		stack: null,
		mailbox: []
	};

	enqueue(process);

	return process;
}

function spawn(task)
{
	return nativeBinding(function(callback) {
		var process = rawSpawn(task);
		callback(succeed(process));
	});
}

function rawSend(process, msg)
{
	process.mailbox.push(msg);
	enqueue(process);
}

function send(process, msg)
{
	return nativeBinding(function(callback) {
		rawSend(process, msg);
		callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function kill(process)
{
	return nativeBinding(function(callback) {
		var root = process.root;
		if (root.ctor === '_Task_nativeBinding' && root.cancel)
		{
			root.cancel();
		}

		process.root = null;

		callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function sleep(time)
{
	return nativeBinding(function(callback) {
		var id = setTimeout(function() {
			callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}


// STEP PROCESSES

function step(numSteps, process)
{
	while (numSteps < MAX_STEPS)
	{
		var ctor = process.root.ctor;

		if (ctor === '_Task_succeed')
		{
			while (process.stack && process.stack.ctor === '_Task_onError')
			{
				process.stack = process.stack.rest;
			}
			if (process.stack === null)
			{
				break;
			}
			process.root = process.stack.callback(process.root.value);
			process.stack = process.stack.rest;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_fail')
		{
			while (process.stack && process.stack.ctor === '_Task_andThen')
			{
				process.stack = process.stack.rest;
			}
			if (process.stack === null)
			{
				break;
			}
			process.root = process.stack.callback(process.root.value);
			process.stack = process.stack.rest;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_andThen')
		{
			process.stack = {
				ctor: '_Task_andThen',
				callback: process.root.callback,
				rest: process.stack
			};
			process.root = process.root.task;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_onError')
		{
			process.stack = {
				ctor: '_Task_onError',
				callback: process.root.callback,
				rest: process.stack
			};
			process.root = process.root.task;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_nativeBinding')
		{
			process.root.cancel = process.root.callback(function(newRoot) {
				process.root = newRoot;
				enqueue(process);
			});

			break;
		}

		if (ctor === '_Task_receive')
		{
			var mailbox = process.mailbox;
			if (mailbox.length === 0)
			{
				break;
			}

			process.root = process.root.callback(mailbox.shift());
			++numSteps;
			continue;
		}

		throw new Error(ctor);
	}

	if (numSteps < MAX_STEPS)
	{
		return numSteps + 1;
	}
	enqueue(process);

	return numSteps;
}


// WORK QUEUE

var working = false;
var workQueue = [];

function enqueue(process)
{
	workQueue.push(process);

	if (!working)
	{
		setTimeout(work, 0);
		working = true;
	}
}

function work()
{
	var numSteps = 0;
	var process;
	while (numSteps < MAX_STEPS && (process = workQueue.shift()))
	{
		if (process.root)
		{
			numSteps = step(numSteps, process);
		}
	}
	if (!process)
	{
		working = false;
		return;
	}
	setTimeout(work, 0);
}


return {
	succeed: succeed,
	fail: fail,
	nativeBinding: nativeBinding,
	andThen: F2(andThen),
	onError: F2(onError),
	receive: receive,

	spawn: spawn,
	kill: kill,
	sleep: sleep,
	send: F2(send),

	rawSpawn: rawSpawn,
	rawSend: rawSend
};

}();
var _elm_lang$core$Platform_Cmd$batch = _elm_lang$core$Native_Platform.batch;
var _elm_lang$core$Platform_Cmd$none = _elm_lang$core$Platform_Cmd$batch(
	{ctor: '[]'});
var _elm_lang$core$Platform_Cmd_ops = _elm_lang$core$Platform_Cmd_ops || {};
_elm_lang$core$Platform_Cmd_ops['!'] = F2(
	function (model, commands) {
		return {
			ctor: '_Tuple2',
			_0: model,
			_1: _elm_lang$core$Platform_Cmd$batch(commands)
		};
	});
var _elm_lang$core$Platform_Cmd$map = _elm_lang$core$Native_Platform.map;
var _elm_lang$core$Platform_Cmd$Cmd = {ctor: 'Cmd'};

var _elm_lang$core$Platform_Sub$batch = _elm_lang$core$Native_Platform.batch;
var _elm_lang$core$Platform_Sub$none = _elm_lang$core$Platform_Sub$batch(
	{ctor: '[]'});
var _elm_lang$core$Platform_Sub$map = _elm_lang$core$Native_Platform.map;
var _elm_lang$core$Platform_Sub$Sub = {ctor: 'Sub'};

var _elm_lang$core$Platform$hack = _elm_lang$core$Native_Scheduler.succeed;
var _elm_lang$core$Platform$sendToSelf = _elm_lang$core$Native_Platform.sendToSelf;
var _elm_lang$core$Platform$sendToApp = _elm_lang$core$Native_Platform.sendToApp;
var _elm_lang$core$Platform$programWithFlags = _elm_lang$core$Native_Platform.programWithFlags;
var _elm_lang$core$Platform$program = _elm_lang$core$Native_Platform.program;
var _elm_lang$core$Platform$Program = {ctor: 'Program'};
var _elm_lang$core$Platform$Task = {ctor: 'Task'};
var _elm_lang$core$Platform$ProcessId = {ctor: 'ProcessId'};
var _elm_lang$core$Platform$Router = {ctor: 'Router'};

var _elm_community$dict_extra$Dict_Extra$find = F2(
	function (predicate, dict) {
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, v, acc) {
					var _p0 = acc;
					if (_p0.ctor === 'Just') {
						return acc;
					} else {
						return A2(predicate, k, v) ? _elm_lang$core$Maybe$Just(
							{ctor: '_Tuple2', _0: k, _1: v}) : _elm_lang$core$Maybe$Nothing;
					}
				}),
			_elm_lang$core$Maybe$Nothing,
			dict);
	});
var _elm_community$dict_extra$Dict_Extra$invert = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldl,
		F3(
			function (k, v, acc) {
				return A3(_elm_lang$core$Dict$insert, v, k, acc);
			}),
		_elm_lang$core$Dict$empty,
		dict);
};
var _elm_community$dict_extra$Dict_Extra$filterMap = F2(
	function (f, dict) {
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, v, acc) {
					var _p1 = A2(f, k, v);
					if (_p1.ctor === 'Just') {
						return A3(_elm_lang$core$Dict$insert, k, _p1._0, acc);
					} else {
						return acc;
					}
				}),
			_elm_lang$core$Dict$empty,
			dict);
	});
var _elm_community$dict_extra$Dict_Extra$mapKeys = F2(
	function (keyMapper, dict) {
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, v, acc) {
					return A3(
						_elm_lang$core$Dict$insert,
						keyMapper(k),
						v,
						acc);
				}),
			_elm_lang$core$Dict$empty,
			dict);
	});
var _elm_community$dict_extra$Dict_Extra$keepOnly = F2(
	function (set, dict) {
		return A3(
			_elm_lang$core$Set$foldl,
			F2(
				function (k, acc) {
					return A2(
						_elm_lang$core$Maybe$withDefault,
						acc,
						A2(
							_elm_lang$core$Maybe$map,
							function (v) {
								return A3(_elm_lang$core$Dict$insert, k, v, acc);
							},
							A2(_elm_lang$core$Dict$get, k, dict)));
				}),
			_elm_lang$core$Dict$empty,
			set);
	});
var _elm_community$dict_extra$Dict_Extra$insertDedupe = F4(
	function (combine, key, value, dict) {
		var $with = function (mbValue) {
			var _p2 = mbValue;
			if (_p2.ctor === 'Just') {
				return _elm_lang$core$Maybe$Just(
					A2(combine, _p2._0, value));
			} else {
				return _elm_lang$core$Maybe$Just(value);
			}
		};
		return A3(_elm_lang$core$Dict$update, key, $with, dict);
	});
var _elm_community$dict_extra$Dict_Extra$removeMany = F2(
	function (set, dict) {
		return A3(_elm_lang$core$Set$foldl, _elm_lang$core$Dict$remove, dict, set);
	});
var _elm_community$dict_extra$Dict_Extra$removeWhen = F2(
	function (pred, dict) {
		return A2(
			_elm_lang$core$Dict$filter,
			F2(
				function (k, v) {
					return !A2(pred, k, v);
				}),
			dict);
	});
var _elm_community$dict_extra$Dict_Extra$fromListDedupeBy = F3(
	function (combine, keyfn, xs) {
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (x, acc) {
					return A4(
						_elm_community$dict_extra$Dict_Extra$insertDedupe,
						combine,
						keyfn(x),
						x,
						acc);
				}),
			_elm_lang$core$Dict$empty,
			xs);
	});
var _elm_community$dict_extra$Dict_Extra$fromListDedupe = F2(
	function (combine, xs) {
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (_p3, acc) {
					var _p4 = _p3;
					return A4(_elm_community$dict_extra$Dict_Extra$insertDedupe, combine, _p4._0, _p4._1, acc);
				}),
			_elm_lang$core$Dict$empty,
			xs);
	});
var _elm_community$dict_extra$Dict_Extra$fromListBy = F2(
	function (keyfn, xs) {
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (x, acc) {
					return A3(
						_elm_lang$core$Dict$insert,
						keyfn(x),
						x,
						acc);
				}),
			_elm_lang$core$Dict$empty,
			xs);
	});
var _elm_community$dict_extra$Dict_Extra$filterGroupBy = F2(
	function (keyfn, list) {
		return A3(
			_elm_lang$core$List$foldr,
			F2(
				function (x, acc) {
					var _p5 = keyfn(x);
					if (_p5.ctor === 'Just') {
						return A3(
							_elm_lang$core$Dict$update,
							_p5._0,
							function (_p6) {
								return _elm_lang$core$Maybe$Just(
									A2(
										_elm_lang$core$Maybe$withDefault,
										{
											ctor: '::',
											_0: x,
											_1: {ctor: '[]'}
										},
										A2(
											_elm_lang$core$Maybe$map,
											F2(
												function (x, y) {
													return {ctor: '::', _0: x, _1: y};
												})(x),
											_p6)));
							},
							acc);
					} else {
						return acc;
					}
				}),
			_elm_lang$core$Dict$empty,
			list);
	});
var _elm_community$dict_extra$Dict_Extra$groupBy = F2(
	function (keyfn, list) {
		return A3(
			_elm_lang$core$List$foldr,
			F2(
				function (x, acc) {
					return A3(
						_elm_lang$core$Dict$update,
						keyfn(x),
						function (_p7) {
							return _elm_lang$core$Maybe$Just(
								A2(
									_elm_lang$core$Maybe$withDefault,
									{
										ctor: '::',
										_0: x,
										_1: {ctor: '[]'}
									},
									A2(
										_elm_lang$core$Maybe$map,
										F2(
											function (x, y) {
												return {ctor: '::', _0: x, _1: y};
											})(x),
										_p7)));
						},
						acc);
				}),
			_elm_lang$core$Dict$empty,
			list);
	});


/*
 * Copyright (c) 2010 Mozilla Corporation
 * Copyright (c) 2010 Vladimir Vukicevic
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */

/*
 * File: mjs
 *
 * Vector and Matrix math utilities for JavaScript, optimized for WebGL.
 * Edited to work with the Elm Programming Language
 */

var _elm_community$linear_algebra$Native_Math_Vector2 = function() {

    var MJS_FLOAT_ARRAY_TYPE = Float32Array;

    var V2 = { };

    if (MJS_FLOAT_ARRAY_TYPE == Array) {
        V2.$ = function V2_$(x, y) {
            return [x, y];
        };
    } else {
        V2.$ = function V2_$(x, y) {
            return new MJS_FLOAT_ARRAY_TYPE([x, y]);
        };
    }

    V2.getX = function V2_getX(a) {
        return a[0];
    }
    V2.getY = function V2_getY(a) {
        return a[1];
    }
    V2.setX = function V2_setX(x, a) {
        return new MJS_FLOAT_ARRAY_TYPE([x, a[1]]);
    }
    V2.setY = function V2_setY(y, a) {
        return new MJS_FLOAT_ARRAY_TYPE([a[0], y]);
    }

    V2.toTuple = function V2_toTuple(a) {
        return {
            ctor:"_Tuple2",
            _0:a[0],
            _1:a[1]
        };
    };
    V2.fromTuple = function V2_fromTuple(t) {
        return new MJS_FLOAT_ARRAY_TYPE([t._0, t._1]);
    };

    V2.toRecord = function V2_toRecord(a) {
        return {
            _:{},
            x:a[0],
            y:a[1]
        };
    };
    V2.fromRecord = function V2_fromRecord(r) {
        return new MJS_FLOAT_ARRAY_TYPE([r.x, r.y]);
    };

    V2.add = function V2_add(a, b) {
        var r = new MJS_FLOAT_ARRAY_TYPE(2);
        r[0] = a[0] + b[0];
        r[1] = a[1] + b[1];
        return r;
    };

    V2.sub = function V2_sub(a, b) {
        var r = new MJS_FLOAT_ARRAY_TYPE(2);
        r[0] = a[0] - b[0];
        r[1] = a[1] - b[1];
        return r;
    };

    V2.neg = function V2_neg(a) {
        var r = new MJS_FLOAT_ARRAY_TYPE(2);
        r[0] = - a[0];
        r[1] = - a[1];
        return r;
    };

    V2.direction = function V2_direction(a, b) {
        var r = new MJS_FLOAT_ARRAY_TYPE(2);
        r[0] = a[0] - b[0];
        r[1] = a[1] - b[1];
        var im = 1.0 / V2.length(r);
        r[0] = r[0] * im;
        r[1] = r[1] * im;
        return r;
    };

    V2.length = function V2_length(a) {
        return Math.sqrt(a[0]*a[0] + a[1]*a[1]);
    };

    V2.lengthSquared = function V2_lengthSquared(a) {
        return a[0]*a[0] + a[1]*a[1];
    };

    V2.distance = function V2_distance(a, b) {
        var dx = a[0] - b[0];
        var dy = a[1] - b[1];
        return Math.sqrt(dx * dx + dy * dy);
    };

    V2.distanceSquared = function V2_distanceSquared(a, b) {
        var dx = a[0] - b[0];
        var dy = a[1] - b[1];
        return dx * dx + dy * dy;
    };

    V2.normalize = function V2_normalize(a) {
        var r = new MJS_FLOAT_ARRAY_TYPE(2);
        var im = 1.0 / V2.length(a);
        r[0] = a[0] * im;
        r[1] = a[1] * im;
        return r;
    };

    V2.scale = function V2_scale(k, a) {
        var r = new MJS_FLOAT_ARRAY_TYPE(2);
        r[0] = a[0] * k;
        r[1] = a[1] * k;
        return r;
    };

    V2.dot = function V2_dot(a, b) {
        return a[0] * b[0] + a[1] * b[1];
    };

    return {
        vec2: F2(V2.$),
        getX: V2.getX,
        getY: V2.getY,
        setX: F2(V2.setX),
        setY: F2(V2.setY),
        toTuple: V2.toTuple,
        toRecord: V2.toRecord,
        fromTuple: V2.fromTuple,
        fromRecord: V2.fromRecord,
        add: F2(V2.add),
        sub: F2(V2.sub),
        neg: V2.neg,
        direction: F2(V2.direction),
        length: V2.length,
        lengthSquared: V2.lengthSquared,
        distance: F2(V2.distance),
        distanceSquared: F2(V2.distanceSquared),
        normalize: V2.normalize,
        scale: F2(V2.scale),
        dot: F2(V2.dot)
    };

}();

var _elm_community$linear_algebra$Math_Vector2$dot = _elm_community$linear_algebra$Native_Math_Vector2.dot;
var _elm_community$linear_algebra$Math_Vector2$scale = _elm_community$linear_algebra$Native_Math_Vector2.scale;
var _elm_community$linear_algebra$Math_Vector2$normalize = _elm_community$linear_algebra$Native_Math_Vector2.normalize;
var _elm_community$linear_algebra$Math_Vector2$distanceSquared = _elm_community$linear_algebra$Native_Math_Vector2.distanceSquared;
var _elm_community$linear_algebra$Math_Vector2$distance = _elm_community$linear_algebra$Native_Math_Vector2.distance;
var _elm_community$linear_algebra$Math_Vector2$lengthSquared = _elm_community$linear_algebra$Native_Math_Vector2.lengthSquared;
var _elm_community$linear_algebra$Math_Vector2$length = _elm_community$linear_algebra$Native_Math_Vector2.length;
var _elm_community$linear_algebra$Math_Vector2$direction = _elm_community$linear_algebra$Native_Math_Vector2.direction;
var _elm_community$linear_algebra$Math_Vector2$negate = _elm_community$linear_algebra$Native_Math_Vector2.neg;
var _elm_community$linear_algebra$Math_Vector2$sub = _elm_community$linear_algebra$Native_Math_Vector2.sub;
var _elm_community$linear_algebra$Math_Vector2$add = _elm_community$linear_algebra$Native_Math_Vector2.add;
var _elm_community$linear_algebra$Math_Vector2$fromRecord = _elm_community$linear_algebra$Native_Math_Vector2.fromRecord;
var _elm_community$linear_algebra$Math_Vector2$fromTuple = _elm_community$linear_algebra$Native_Math_Vector2.fromTuple;
var _elm_community$linear_algebra$Math_Vector2$toRecord = _elm_community$linear_algebra$Native_Math_Vector2.toRecord;
var _elm_community$linear_algebra$Math_Vector2$toTuple = _elm_community$linear_algebra$Native_Math_Vector2.toTuple;
var _elm_community$linear_algebra$Math_Vector2$setY = _elm_community$linear_algebra$Native_Math_Vector2.setY;
var _elm_community$linear_algebra$Math_Vector2$setX = _elm_community$linear_algebra$Native_Math_Vector2.setX;
var _elm_community$linear_algebra$Math_Vector2$getY = _elm_community$linear_algebra$Native_Math_Vector2.getY;
var _elm_community$linear_algebra$Math_Vector2$getX = _elm_community$linear_algebra$Native_Math_Vector2.getX;
var _elm_community$linear_algebra$Math_Vector2$vec2 = _elm_community$linear_algebra$Native_Math_Vector2.vec2;
var _elm_community$linear_algebra$Math_Vector2$Vec2 = {ctor: 'Vec2'};

var _elm_community$list_extra$List_Extra$greedyGroupsOfWithStep = F3(
	function (size, step, xs) {
		var okayXs = _elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$List$length(xs),
			0) > 0;
		var okayArgs = (_elm_lang$core$Native_Utils.cmp(size, 0) > 0) && (_elm_lang$core$Native_Utils.cmp(step, 0) > 0);
		var xs_ = A2(_elm_lang$core$List$drop, step, xs);
		var group = A2(_elm_lang$core$List$take, size, xs);
		return (okayArgs && okayXs) ? {
			ctor: '::',
			_0: group,
			_1: A3(_elm_community$list_extra$List_Extra$greedyGroupsOfWithStep, size, step, xs_)
		} : {ctor: '[]'};
	});
var _elm_community$list_extra$List_Extra$greedyGroupsOf = F2(
	function (size, xs) {
		return A3(_elm_community$list_extra$List_Extra$greedyGroupsOfWithStep, size, size, xs);
	});
var _elm_community$list_extra$List_Extra$groupsOfWithStep = F3(
	function (size, step, xs) {
		var okayArgs = (_elm_lang$core$Native_Utils.cmp(size, 0) > 0) && (_elm_lang$core$Native_Utils.cmp(step, 0) > 0);
		var xs_ = A2(_elm_lang$core$List$drop, step, xs);
		var group = A2(_elm_lang$core$List$take, size, xs);
		var okayLength = _elm_lang$core$Native_Utils.eq(
			size,
			_elm_lang$core$List$length(group));
		return (okayArgs && okayLength) ? {
			ctor: '::',
			_0: group,
			_1: A3(_elm_community$list_extra$List_Extra$groupsOfWithStep, size, step, xs_)
		} : {ctor: '[]'};
	});
var _elm_community$list_extra$List_Extra$groupsOf = F2(
	function (size, xs) {
		return A3(_elm_community$list_extra$List_Extra$groupsOfWithStep, size, size, xs);
	});
var _elm_community$list_extra$List_Extra$zip5 = _elm_lang$core$List$map5(
	F5(
		function (v0, v1, v2, v3, v4) {
			return {ctor: '_Tuple5', _0: v0, _1: v1, _2: v2, _3: v3, _4: v4};
		}));
var _elm_community$list_extra$List_Extra$zip4 = _elm_lang$core$List$map4(
	F4(
		function (v0, v1, v2, v3) {
			return {ctor: '_Tuple4', _0: v0, _1: v1, _2: v2, _3: v3};
		}));
var _elm_community$list_extra$List_Extra$zip3 = _elm_lang$core$List$map3(
	F3(
		function (v0, v1, v2) {
			return {ctor: '_Tuple3', _0: v0, _1: v1, _2: v2};
		}));
var _elm_community$list_extra$List_Extra$zip = _elm_lang$core$List$map2(
	F2(
		function (v0, v1) {
			return {ctor: '_Tuple2', _0: v0, _1: v1};
		}));
var _elm_community$list_extra$List_Extra$isPrefixOf = F2(
	function (prefix, xs) {
		var _p0 = {ctor: '_Tuple2', _0: prefix, _1: xs};
		if (_p0._0.ctor === '[]') {
			return true;
		} else {
			if (_p0._1.ctor === '[]') {
				return false;
			} else {
				return _elm_lang$core$Native_Utils.eq(_p0._0._0, _p0._1._0) && A2(_elm_community$list_extra$List_Extra$isPrefixOf, _p0._0._1, _p0._1._1);
			}
		}
	});
var _elm_community$list_extra$List_Extra$isSuffixOf = F2(
	function (suffix, xs) {
		return A2(
			_elm_community$list_extra$List_Extra$isPrefixOf,
			_elm_lang$core$List$reverse(suffix),
			_elm_lang$core$List$reverse(xs));
	});
var _elm_community$list_extra$List_Extra$selectSplit = function (xs) {
	var _p1 = xs;
	if (_p1.ctor === '[]') {
		return {ctor: '[]'};
	} else {
		var _p5 = _p1._1;
		var _p4 = _p1._0;
		return {
			ctor: '::',
			_0: {
				ctor: '_Tuple3',
				_0: {ctor: '[]'},
				_1: _p4,
				_2: _p5
			},
			_1: A2(
				_elm_lang$core$List$map,
				function (_p2) {
					var _p3 = _p2;
					return {
						ctor: '_Tuple3',
						_0: {ctor: '::', _0: _p4, _1: _p3._0},
						_1: _p3._1,
						_2: _p3._2
					};
				},
				_elm_community$list_extra$List_Extra$selectSplit(_p5))
		};
	}
};
var _elm_community$list_extra$List_Extra$select = function (xs) {
	var _p6 = xs;
	if (_p6.ctor === '[]') {
		return {ctor: '[]'};
	} else {
		var _p10 = _p6._1;
		var _p9 = _p6._0;
		return {
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: _p9, _1: _p10},
			_1: A2(
				_elm_lang$core$List$map,
				function (_p7) {
					var _p8 = _p7;
					return {
						ctor: '_Tuple2',
						_0: _p8._0,
						_1: {ctor: '::', _0: _p9, _1: _p8._1}
					};
				},
				_elm_community$list_extra$List_Extra$select(_p10))
		};
	}
};
var _elm_community$list_extra$List_Extra$tailsHelp = F2(
	function (e, list) {
		var _p11 = list;
		if (_p11.ctor === '::') {
			var _p12 = _p11._0;
			return {
				ctor: '::',
				_0: {ctor: '::', _0: e, _1: _p12},
				_1: {ctor: '::', _0: _p12, _1: _p11._1}
			};
		} else {
			return {ctor: '[]'};
		}
	});
var _elm_community$list_extra$List_Extra$tails = A2(
	_elm_lang$core$List$foldr,
	_elm_community$list_extra$List_Extra$tailsHelp,
	{
		ctor: '::',
		_0: {ctor: '[]'},
		_1: {ctor: '[]'}
	});
var _elm_community$list_extra$List_Extra$isInfixOf = F2(
	function (infix, xs) {
		return A2(
			_elm_lang$core$List$any,
			_elm_community$list_extra$List_Extra$isPrefixOf(infix),
			_elm_community$list_extra$List_Extra$tails(xs));
	});
var _elm_community$list_extra$List_Extra$inits = A2(
	_elm_lang$core$List$foldr,
	F2(
		function (e, acc) {
			return {
				ctor: '::',
				_0: {ctor: '[]'},
				_1: A2(
					_elm_lang$core$List$map,
					F2(
						function (x, y) {
							return {ctor: '::', _0: x, _1: y};
						})(e),
					acc)
			};
		}),
	{
		ctor: '::',
		_0: {ctor: '[]'},
		_1: {ctor: '[]'}
	});
var _elm_community$list_extra$List_Extra$groupWhileTransitively = F2(
	function (cmp, xs_) {
		var _p13 = xs_;
		if (_p13.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			if (_p13._1.ctor === '[]') {
				return {
					ctor: '::',
					_0: {
						ctor: '::',
						_0: _p13._0,
						_1: {ctor: '[]'}
					},
					_1: {ctor: '[]'}
				};
			} else {
				var _p15 = _p13._0;
				var _p14 = A2(_elm_community$list_extra$List_Extra$groupWhileTransitively, cmp, _p13._1);
				if (_p14.ctor === '::') {
					return A2(cmp, _p15, _p13._1._0) ? {
						ctor: '::',
						_0: {ctor: '::', _0: _p15, _1: _p14._0},
						_1: _p14._1
					} : {
						ctor: '::',
						_0: {
							ctor: '::',
							_0: _p15,
							_1: {ctor: '[]'}
						},
						_1: _p14
					};
				} else {
					return {ctor: '[]'};
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$stripPrefix = F2(
	function (prefix, xs) {
		var step = F2(
			function (e, m) {
				var _p16 = m;
				if (_p16.ctor === 'Nothing') {
					return _elm_lang$core$Maybe$Nothing;
				} else {
					if (_p16._0.ctor === '[]') {
						return _elm_lang$core$Maybe$Nothing;
					} else {
						return _elm_lang$core$Native_Utils.eq(e, _p16._0._0) ? _elm_lang$core$Maybe$Just(_p16._0._1) : _elm_lang$core$Maybe$Nothing;
					}
				}
			});
		return A3(
			_elm_lang$core$List$foldl,
			step,
			_elm_lang$core$Maybe$Just(xs),
			prefix);
	});
var _elm_community$list_extra$List_Extra$dropWhileRight = function (p) {
	return A2(
		_elm_lang$core$List$foldr,
		F2(
			function (x, xs) {
				return (p(x) && _elm_lang$core$List$isEmpty(xs)) ? {ctor: '[]'} : {ctor: '::', _0: x, _1: xs};
			}),
		{ctor: '[]'});
};
var _elm_community$list_extra$List_Extra$takeWhileRight = function (p) {
	var step = F2(
		function (x, _p17) {
			var _p18 = _p17;
			var _p19 = _p18._0;
			return (p(x) && _p18._1) ? {
				ctor: '_Tuple2',
				_0: {ctor: '::', _0: x, _1: _p19},
				_1: true
			} : {ctor: '_Tuple2', _0: _p19, _1: false};
		});
	return function (_p20) {
		return _elm_lang$core$Tuple$first(
			A3(
				_elm_lang$core$List$foldr,
				step,
				{
					ctor: '_Tuple2',
					_0: {ctor: '[]'},
					_1: true
				},
				_p20));
	};
};
var _elm_community$list_extra$List_Extra$splitAt = F2(
	function (n, xs) {
		return {
			ctor: '_Tuple2',
			_0: A2(_elm_lang$core$List$take, n, xs),
			_1: A2(_elm_lang$core$List$drop, n, xs)
		};
	});
var _elm_community$list_extra$List_Extra$groupsOfVarying_ = F3(
	function (listOflengths, list, accu) {
		groupsOfVarying_:
		while (true) {
			var _p21 = {ctor: '_Tuple2', _0: listOflengths, _1: list};
			if (((_p21.ctor === '_Tuple2') && (_p21._0.ctor === '::')) && (_p21._1.ctor === '::')) {
				var _p22 = A2(_elm_community$list_extra$List_Extra$splitAt, _p21._0._0, list);
				var head = _p22._0;
				var tail = _p22._1;
				var _v11 = _p21._0._1,
					_v12 = tail,
					_v13 = {ctor: '::', _0: head, _1: accu};
				listOflengths = _v11;
				list = _v12;
				accu = _v13;
				continue groupsOfVarying_;
			} else {
				return _elm_lang$core$List$reverse(accu);
			}
		}
	});
var _elm_community$list_extra$List_Extra$groupsOfVarying = F2(
	function (listOflengths, list) {
		return A3(
			_elm_community$list_extra$List_Extra$groupsOfVarying_,
			listOflengths,
			list,
			{ctor: '[]'});
	});
var _elm_community$list_extra$List_Extra$unfoldr = F2(
	function (f, seed) {
		var _p23 = f(seed);
		if (_p23.ctor === 'Nothing') {
			return {ctor: '[]'};
		} else {
			return {
				ctor: '::',
				_0: _p23._0._0,
				_1: A2(_elm_community$list_extra$List_Extra$unfoldr, f, _p23._0._1)
			};
		}
	});
var _elm_community$list_extra$List_Extra$scanr1 = F2(
	function (f, xs_) {
		var _p24 = xs_;
		if (_p24.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			if (_p24._1.ctor === '[]') {
				return {
					ctor: '::',
					_0: _p24._0,
					_1: {ctor: '[]'}
				};
			} else {
				var _p25 = A2(_elm_community$list_extra$List_Extra$scanr1, f, _p24._1);
				if (_p25.ctor === '::') {
					return {
						ctor: '::',
						_0: A2(f, _p24._0, _p25._0),
						_1: _p25
					};
				} else {
					return {ctor: '[]'};
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$scanr = F3(
	function (f, acc, xs_) {
		var _p26 = xs_;
		if (_p26.ctor === '[]') {
			return {
				ctor: '::',
				_0: acc,
				_1: {ctor: '[]'}
			};
		} else {
			var _p27 = A3(_elm_community$list_extra$List_Extra$scanr, f, acc, _p26._1);
			if (_p27.ctor === '::') {
				return {
					ctor: '::',
					_0: A2(f, _p26._0, _p27._0),
					_1: _p27
				};
			} else {
				return {ctor: '[]'};
			}
		}
	});
var _elm_community$list_extra$List_Extra$scanl1 = F2(
	function (f, xs_) {
		var _p28 = xs_;
		if (_p28.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			return A3(_elm_lang$core$List$scanl, f, _p28._0, _p28._1);
		}
	});
var _elm_community$list_extra$List_Extra$indexedFoldr = F3(
	function (func, acc, list) {
		var step = F2(
			function (x, _p29) {
				var _p30 = _p29;
				var _p31 = _p30._0;
				return {
					ctor: '_Tuple2',
					_0: _p31 - 1,
					_1: A3(func, _p31, x, _p30._1)
				};
			});
		return _elm_lang$core$Tuple$second(
			A3(
				_elm_lang$core$List$foldr,
				step,
				{
					ctor: '_Tuple2',
					_0: _elm_lang$core$List$length(list) - 1,
					_1: acc
				},
				list));
	});
var _elm_community$list_extra$List_Extra$indexedFoldl = F3(
	function (func, acc, list) {
		var step = F2(
			function (x, _p32) {
				var _p33 = _p32;
				var _p34 = _p33._0;
				return {
					ctor: '_Tuple2',
					_0: _p34 + 1,
					_1: A3(func, _p34, x, _p33._1)
				};
			});
		return _elm_lang$core$Tuple$second(
			A3(
				_elm_lang$core$List$foldl,
				step,
				{ctor: '_Tuple2', _0: 0, _1: acc},
				list));
	});
var _elm_community$list_extra$List_Extra$foldr1 = F2(
	function (f, xs) {
		var mf = F2(
			function (x, m) {
				return _elm_lang$core$Maybe$Just(
					function () {
						var _p35 = m;
						if (_p35.ctor === 'Nothing') {
							return x;
						} else {
							return A2(f, x, _p35._0);
						}
					}());
			});
		return A3(_elm_lang$core$List$foldr, mf, _elm_lang$core$Maybe$Nothing, xs);
	});
var _elm_community$list_extra$List_Extra$foldl1 = F2(
	function (f, xs) {
		var mf = F2(
			function (x, m) {
				return _elm_lang$core$Maybe$Just(
					function () {
						var _p36 = m;
						if (_p36.ctor === 'Nothing') {
							return x;
						} else {
							return A2(f, _p36._0, x);
						}
					}());
			});
		return A3(_elm_lang$core$List$foldl, mf, _elm_lang$core$Maybe$Nothing, xs);
	});
var _elm_community$list_extra$List_Extra$interweaveHelp = F3(
	function (l1, l2, acc) {
		interweaveHelp:
		while (true) {
			var _p37 = {ctor: '_Tuple2', _0: l1, _1: l2};
			_v24_1:
			do {
				if (_p37._0.ctor === '::') {
					if (_p37._1.ctor === '::') {
						var _v25 = _p37._0._1,
							_v26 = _p37._1._1,
							_v27 = A2(
							_elm_lang$core$Basics_ops['++'],
							acc,
							{
								ctor: '::',
								_0: _p37._0._0,
								_1: {
									ctor: '::',
									_0: _p37._1._0,
									_1: {ctor: '[]'}
								}
							});
						l1 = _v25;
						l2 = _v26;
						acc = _v27;
						continue interweaveHelp;
					} else {
						break _v24_1;
					}
				} else {
					if (_p37._1.ctor === '[]') {
						break _v24_1;
					} else {
						return A2(_elm_lang$core$Basics_ops['++'], acc, _p37._1);
					}
				}
			} while(false);
			return A2(_elm_lang$core$Basics_ops['++'], acc, _p37._0);
		}
	});
var _elm_community$list_extra$List_Extra$interweave = F2(
	function (l1, l2) {
		return A3(
			_elm_community$list_extra$List_Extra$interweaveHelp,
			l1,
			l2,
			{ctor: '[]'});
	});
var _elm_community$list_extra$List_Extra$permutations = function (xs_) {
	var _p38 = xs_;
	if (_p38.ctor === '[]') {
		return {
			ctor: '::',
			_0: {ctor: '[]'},
			_1: {ctor: '[]'}
		};
	} else {
		var f = function (_p39) {
			var _p40 = _p39;
			return A2(
				_elm_lang$core$List$map,
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					})(_p40._0),
				_elm_community$list_extra$List_Extra$permutations(_p40._1));
		};
		return A2(
			_elm_lang$core$List$concatMap,
			f,
			_elm_community$list_extra$List_Extra$select(_p38));
	}
};
var _elm_community$list_extra$List_Extra$isPermutationOf = F2(
	function (permut, xs) {
		return A2(
			_elm_lang$core$List$member,
			permut,
			_elm_community$list_extra$List_Extra$permutations(xs));
	});
var _elm_community$list_extra$List_Extra$subsequencesNonEmpty = function (xs) {
	var _p41 = xs;
	if (_p41.ctor === '[]') {
		return {ctor: '[]'};
	} else {
		var _p42 = _p41._0;
		var f = F2(
			function (ys, r) {
				return {
					ctor: '::',
					_0: ys,
					_1: {
						ctor: '::',
						_0: {ctor: '::', _0: _p42, _1: ys},
						_1: r
					}
				};
			});
		return {
			ctor: '::',
			_0: {
				ctor: '::',
				_0: _p42,
				_1: {ctor: '[]'}
			},
			_1: A3(
				_elm_lang$core$List$foldr,
				f,
				{ctor: '[]'},
				_elm_community$list_extra$List_Extra$subsequencesNonEmpty(_p41._1))
		};
	}
};
var _elm_community$list_extra$List_Extra$subsequences = function (xs) {
	return {
		ctor: '::',
		_0: {ctor: '[]'},
		_1: _elm_community$list_extra$List_Extra$subsequencesNonEmpty(xs)
	};
};
var _elm_community$list_extra$List_Extra$isSubsequenceOf = F2(
	function (subseq, xs) {
		return A2(
			_elm_lang$core$List$member,
			subseq,
			_elm_community$list_extra$List_Extra$subsequences(xs));
	});
var _elm_community$list_extra$List_Extra$transpose = function (ll) {
	transpose:
	while (true) {
		var _p43 = ll;
		if (_p43.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			if (_p43._0.ctor === '[]') {
				var _v32 = _p43._1;
				ll = _v32;
				continue transpose;
			} else {
				var _p44 = _p43._1;
				var tails = A2(_elm_lang$core$List$filterMap, _elm_lang$core$List$tail, _p44);
				var heads = A2(_elm_lang$core$List$filterMap, _elm_lang$core$List$head, _p44);
				return {
					ctor: '::',
					_0: {ctor: '::', _0: _p43._0._0, _1: heads},
					_1: _elm_community$list_extra$List_Extra$transpose(
						{ctor: '::', _0: _p43._0._1, _1: tails})
				};
			}
		}
	}
};
var _elm_community$list_extra$List_Extra$intercalate = function (xs) {
	return function (_p45) {
		return _elm_lang$core$List$concat(
			A2(_elm_lang$core$List$intersperse, xs, _p45));
	};
};
var _elm_community$list_extra$List_Extra$filterNot = F2(
	function (pred, list) {
		return A2(
			_elm_lang$core$List$filter,
			function (_p46) {
				return !pred(_p46);
			},
			list);
	});
var _elm_community$list_extra$List_Extra$removeAt = F2(
	function (index, l) {
		if (_elm_lang$core$Native_Utils.cmp(index, 0) < 0) {
			return l;
		} else {
			var tail = _elm_lang$core$List$tail(
				A2(_elm_lang$core$List$drop, index, l));
			var head = A2(_elm_lang$core$List$take, index, l);
			var _p47 = tail;
			if (_p47.ctor === 'Nothing') {
				return l;
			} else {
				return A2(_elm_lang$core$List$append, head, _p47._0);
			}
		}
	});
var _elm_community$list_extra$List_Extra$stableSortWith = F2(
	function (pred, list) {
		var predWithIndex = F2(
			function (_p49, _p48) {
				var _p50 = _p49;
				var _p51 = _p48;
				var result = A2(pred, _p50._0, _p51._0);
				var _p52 = result;
				if (_p52.ctor === 'EQ') {
					return A2(_elm_lang$core$Basics$compare, _p50._1, _p51._1);
				} else {
					return result;
				}
			});
		var listWithIndex = A2(
			_elm_lang$core$List$indexedMap,
			F2(
				function (i, a) {
					return {ctor: '_Tuple2', _0: a, _1: i};
				}),
			list);
		return A2(
			_elm_lang$core$List$map,
			_elm_lang$core$Tuple$first,
			A2(_elm_lang$core$List$sortWith, predWithIndex, listWithIndex));
	});
var _elm_community$list_extra$List_Extra$setAt = F3(
	function (index, value, l) {
		if (_elm_lang$core$Native_Utils.cmp(index, 0) < 0) {
			return _elm_lang$core$Maybe$Nothing;
		} else {
			var tail = _elm_lang$core$List$tail(
				A2(_elm_lang$core$List$drop, index, l));
			var head = A2(_elm_lang$core$List$take, index, l);
			var _p53 = tail;
			if (_p53.ctor === 'Nothing') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				return _elm_lang$core$Maybe$Just(
					A2(
						_elm_lang$core$List$append,
						head,
						{ctor: '::', _0: value, _1: _p53._0}));
			}
		}
	});
var _elm_community$list_extra$List_Extra$remove = F2(
	function (x, xs) {
		var _p54 = xs;
		if (_p54.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var _p56 = _p54._1;
			var _p55 = _p54._0;
			return _elm_lang$core$Native_Utils.eq(x, _p55) ? _p56 : {
				ctor: '::',
				_0: _p55,
				_1: A2(_elm_community$list_extra$List_Extra$remove, x, _p56)
			};
		}
	});
var _elm_community$list_extra$List_Extra$updateIfIndex = F3(
	function (predicate, update, list) {
		return A2(
			_elm_lang$core$List$indexedMap,
			F2(
				function (i, x) {
					return predicate(i) ? update(x) : x;
				}),
			list);
	});
var _elm_community$list_extra$List_Extra$updateAt = F3(
	function (index, update, list) {
		return ((_elm_lang$core$Native_Utils.cmp(index, 0) < 0) || (_elm_lang$core$Native_Utils.cmp(
			index,
			_elm_lang$core$List$length(list)) > -1)) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(
			A3(
				_elm_community$list_extra$List_Extra$updateIfIndex,
				F2(
					function (x, y) {
						return _elm_lang$core$Native_Utils.eq(x, y);
					})(index),
				update,
				list));
	});
var _elm_community$list_extra$List_Extra$updateIf = F3(
	function (predicate, update, list) {
		return A2(
			_elm_lang$core$List$map,
			function (item) {
				return predicate(item) ? update(item) : item;
			},
			list);
	});
var _elm_community$list_extra$List_Extra$replaceIf = F3(
	function (predicate, replacement, list) {
		return A3(
			_elm_community$list_extra$List_Extra$updateIf,
			predicate,
			_elm_lang$core$Basics$always(replacement),
			list);
	});
var _elm_community$list_extra$List_Extra$findIndices = function (p) {
	return function (_p57) {
		return A2(
			_elm_lang$core$List$map,
			_elm_lang$core$Tuple$first,
			A2(
				_elm_lang$core$List$filter,
				function (_p58) {
					var _p59 = _p58;
					return p(_p59._1);
				},
				A2(
					_elm_lang$core$List$indexedMap,
					F2(
						function (v0, v1) {
							return {ctor: '_Tuple2', _0: v0, _1: v1};
						}),
					_p57)));
	};
};
var _elm_community$list_extra$List_Extra$findIndex = function (p) {
	return function (_p60) {
		return _elm_lang$core$List$head(
			A2(_elm_community$list_extra$List_Extra$findIndices, p, _p60));
	};
};
var _elm_community$list_extra$List_Extra$splitWhen = F2(
	function (predicate, list) {
		return A2(
			_elm_lang$core$Maybe$map,
			function (i) {
				return A2(_elm_community$list_extra$List_Extra$splitAt, i, list);
			},
			A2(_elm_community$list_extra$List_Extra$findIndex, predicate, list));
	});
var _elm_community$list_extra$List_Extra$elemIndices = function (x) {
	return _elm_community$list_extra$List_Extra$findIndices(
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.eq(x, y);
			})(x));
};
var _elm_community$list_extra$List_Extra$elemIndex = function (x) {
	return _elm_community$list_extra$List_Extra$findIndex(
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.eq(x, y);
			})(x));
};
var _elm_community$list_extra$List_Extra$find = F2(
	function (predicate, list) {
		find:
		while (true) {
			var _p61 = list;
			if (_p61.ctor === '[]') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				var _p62 = _p61._0;
				if (predicate(_p62)) {
					return _elm_lang$core$Maybe$Just(_p62);
				} else {
					var _v41 = predicate,
						_v42 = _p61._1;
					predicate = _v41;
					list = _v42;
					continue find;
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$notMember = function (x) {
	return function (_p63) {
		return !A2(_elm_lang$core$List$member, x, _p63);
	};
};
var _elm_community$list_extra$List_Extra$andThen = _elm_lang$core$List$concatMap;
var _elm_community$list_extra$List_Extra$lift2 = F3(
	function (f, la, lb) {
		return A2(
			_elm_community$list_extra$List_Extra$andThen,
			function (a) {
				return A2(
					_elm_community$list_extra$List_Extra$andThen,
					function (b) {
						return {
							ctor: '::',
							_0: A2(f, a, b),
							_1: {ctor: '[]'}
						};
					},
					lb);
			},
			la);
	});
var _elm_community$list_extra$List_Extra$lift3 = F4(
	function (f, la, lb, lc) {
		return A2(
			_elm_community$list_extra$List_Extra$andThen,
			function (a) {
				return A2(
					_elm_community$list_extra$List_Extra$andThen,
					function (b) {
						return A2(
							_elm_community$list_extra$List_Extra$andThen,
							function (c) {
								return {
									ctor: '::',
									_0: A3(f, a, b, c),
									_1: {ctor: '[]'}
								};
							},
							lc);
					},
					lb);
			},
			la);
	});
var _elm_community$list_extra$List_Extra$lift4 = F5(
	function (f, la, lb, lc, ld) {
		return A2(
			_elm_community$list_extra$List_Extra$andThen,
			function (a) {
				return A2(
					_elm_community$list_extra$List_Extra$andThen,
					function (b) {
						return A2(
							_elm_community$list_extra$List_Extra$andThen,
							function (c) {
								return A2(
									_elm_community$list_extra$List_Extra$andThen,
									function (d) {
										return {
											ctor: '::',
											_0: A4(f, a, b, c, d),
											_1: {ctor: '[]'}
										};
									},
									ld);
							},
							lc);
					},
					lb);
			},
			la);
	});
var _elm_community$list_extra$List_Extra$andMap = F2(
	function (l, fl) {
		return A3(
			_elm_lang$core$List$map2,
			F2(
				function (x, y) {
					return x(y);
				}),
			fl,
			l);
	});
var _elm_community$list_extra$List_Extra$uniqueHelp = F3(
	function (f, existing, remaining) {
		uniqueHelp:
		while (true) {
			var _p64 = remaining;
			if (_p64.ctor === '[]') {
				return {ctor: '[]'};
			} else {
				var _p66 = _p64._1;
				var _p65 = _p64._0;
				var computedFirst = f(_p65);
				if (A2(_elm_lang$core$Set$member, computedFirst, existing)) {
					var _v44 = f,
						_v45 = existing,
						_v46 = _p66;
					f = _v44;
					existing = _v45;
					remaining = _v46;
					continue uniqueHelp;
				} else {
					return {
						ctor: '::',
						_0: _p65,
						_1: A3(
							_elm_community$list_extra$List_Extra$uniqueHelp,
							f,
							A2(_elm_lang$core$Set$insert, computedFirst, existing),
							_p66)
					};
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$uniqueBy = F2(
	function (f, list) {
		return A3(_elm_community$list_extra$List_Extra$uniqueHelp, f, _elm_lang$core$Set$empty, list);
	});
var _elm_community$list_extra$List_Extra$allDifferentBy = F2(
	function (f, list) {
		return _elm_lang$core$Native_Utils.eq(
			_elm_lang$core$List$length(list),
			_elm_lang$core$List$length(
				A2(_elm_community$list_extra$List_Extra$uniqueBy, f, list)));
	});
var _elm_community$list_extra$List_Extra$allDifferent = function (list) {
	return A2(_elm_community$list_extra$List_Extra$allDifferentBy, _elm_lang$core$Basics$identity, list);
};
var _elm_community$list_extra$List_Extra$unique = function (list) {
	return A3(_elm_community$list_extra$List_Extra$uniqueHelp, _elm_lang$core$Basics$identity, _elm_lang$core$Set$empty, list);
};
var _elm_community$list_extra$List_Extra$dropWhile = F2(
	function (predicate, list) {
		dropWhile:
		while (true) {
			var _p67 = list;
			if (_p67.ctor === '[]') {
				return {ctor: '[]'};
			} else {
				if (predicate(_p67._0)) {
					var _v48 = predicate,
						_v49 = _p67._1;
					predicate = _v48;
					list = _v49;
					continue dropWhile;
				} else {
					return list;
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$takeWhile = function (predicate) {
	var takeWhileMemo = F2(
		function (memo, list) {
			takeWhileMemo:
			while (true) {
				var _p68 = list;
				if (_p68.ctor === '[]') {
					return _elm_lang$core$List$reverse(memo);
				} else {
					var _p69 = _p68._0;
					if (predicate(_p69)) {
						var _v51 = {ctor: '::', _0: _p69, _1: memo},
							_v52 = _p68._1;
						memo = _v51;
						list = _v52;
						continue takeWhileMemo;
					} else {
						return _elm_lang$core$List$reverse(memo);
					}
				}
			}
		});
	return takeWhileMemo(
		{ctor: '[]'});
};
var _elm_community$list_extra$List_Extra$span = F2(
	function (p, xs) {
		return {
			ctor: '_Tuple2',
			_0: A2(_elm_community$list_extra$List_Extra$takeWhile, p, xs),
			_1: A2(_elm_community$list_extra$List_Extra$dropWhile, p, xs)
		};
	});
var _elm_community$list_extra$List_Extra$break = function (p) {
	return _elm_community$list_extra$List_Extra$span(
		function (_p70) {
			return !p(_p70);
		});
};
var _elm_community$list_extra$List_Extra$groupWhile = F2(
	function (eq, xs_) {
		var _p71 = xs_;
		if (_p71.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var _p73 = _p71._0;
			var _p72 = A2(
				_elm_community$list_extra$List_Extra$span,
				eq(_p73),
				_p71._1);
			var ys = _p72._0;
			var zs = _p72._1;
			return {
				ctor: '::',
				_0: {ctor: '::', _0: _p73, _1: ys},
				_1: A2(_elm_community$list_extra$List_Extra$groupWhile, eq, zs)
			};
		}
	});
var _elm_community$list_extra$List_Extra$group = _elm_community$list_extra$List_Extra$groupWhile(
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.eq(x, y);
		}));
var _elm_community$list_extra$List_Extra$minimumBy = F2(
	function (f, ls) {
		var minBy = F2(
			function (x, _p74) {
				var _p75 = _p74;
				var _p76 = _p75._1;
				var fx = f(x);
				return (_elm_lang$core$Native_Utils.cmp(fx, _p76) < 0) ? {ctor: '_Tuple2', _0: x, _1: fx} : {ctor: '_Tuple2', _0: _p75._0, _1: _p76};
			});
		var _p77 = ls;
		if (_p77.ctor === '::') {
			if (_p77._1.ctor === '[]') {
				return _elm_lang$core$Maybe$Just(_p77._0);
			} else {
				var _p78 = _p77._0;
				return _elm_lang$core$Maybe$Just(
					_elm_lang$core$Tuple$first(
						A3(
							_elm_lang$core$List$foldl,
							minBy,
							{
								ctor: '_Tuple2',
								_0: _p78,
								_1: f(_p78)
							},
							_p77._1)));
			}
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_community$list_extra$List_Extra$maximumBy = F2(
	function (f, ls) {
		var maxBy = F2(
			function (x, _p79) {
				var _p80 = _p79;
				var _p81 = _p80._1;
				var fx = f(x);
				return (_elm_lang$core$Native_Utils.cmp(fx, _p81) > 0) ? {ctor: '_Tuple2', _0: x, _1: fx} : {ctor: '_Tuple2', _0: _p80._0, _1: _p81};
			});
		var _p82 = ls;
		if (_p82.ctor === '::') {
			if (_p82._1.ctor === '[]') {
				return _elm_lang$core$Maybe$Just(_p82._0);
			} else {
				var _p83 = _p82._0;
				return _elm_lang$core$Maybe$Just(
					_elm_lang$core$Tuple$first(
						A3(
							_elm_lang$core$List$foldl,
							maxBy,
							{
								ctor: '_Tuple2',
								_0: _p83,
								_1: f(_p83)
							},
							_p82._1)));
			}
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_community$list_extra$List_Extra$uncons = function (xs) {
	var _p84 = xs;
	if (_p84.ctor === '[]') {
		return _elm_lang$core$Maybe$Nothing;
	} else {
		return _elm_lang$core$Maybe$Just(
			{ctor: '_Tuple2', _0: _p84._0, _1: _p84._1});
	}
};
var _elm_community$list_extra$List_Extra$swapAt = F3(
	function (index1, index2, l) {
		swapAt:
		while (true) {
			if (_elm_lang$core$Native_Utils.eq(index1, index2)) {
				return _elm_lang$core$Maybe$Just(l);
			} else {
				if (_elm_lang$core$Native_Utils.cmp(index1, index2) > 0) {
					var _v59 = index2,
						_v60 = index1,
						_v61 = l;
					index1 = _v59;
					index2 = _v60;
					l = _v61;
					continue swapAt;
				} else {
					if (_elm_lang$core$Native_Utils.cmp(index1, 0) < 0) {
						return _elm_lang$core$Maybe$Nothing;
					} else {
						var _p85 = A2(_elm_community$list_extra$List_Extra$splitAt, index1, l);
						var part1 = _p85._0;
						var tail1 = _p85._1;
						var _p86 = A2(_elm_community$list_extra$List_Extra$splitAt, index2 - index1, tail1);
						var head2 = _p86._0;
						var tail2 = _p86._1;
						return A3(
							_elm_lang$core$Maybe$map2,
							F2(
								function (_p88, _p87) {
									var _p89 = _p88;
									var _p90 = _p87;
									return _elm_lang$core$List$concat(
										{
											ctor: '::',
											_0: part1,
											_1: {
												ctor: '::',
												_0: {ctor: '::', _0: _p90._0, _1: _p89._1},
												_1: {
													ctor: '::',
													_0: {ctor: '::', _0: _p89._0, _1: _p90._1},
													_1: {ctor: '[]'}
												}
											}
										});
								}),
							_elm_community$list_extra$List_Extra$uncons(head2),
							_elm_community$list_extra$List_Extra$uncons(tail2));
					}
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$iterate = F2(
	function (f, x) {
		var _p91 = f(x);
		if (_p91.ctor === 'Just') {
			return {
				ctor: '::',
				_0: x,
				_1: A2(_elm_community$list_extra$List_Extra$iterate, f, _p91._0)
			};
		} else {
			return {
				ctor: '::',
				_0: x,
				_1: {ctor: '[]'}
			};
		}
	});
var _elm_community$list_extra$List_Extra$getAt = F2(
	function (idx, xs) {
		return (_elm_lang$core$Native_Utils.cmp(idx, 0) < 0) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$List$head(
			A2(_elm_lang$core$List$drop, idx, xs));
	});
var _elm_community$list_extra$List_Extra_ops = _elm_community$list_extra$List_Extra_ops || {};
_elm_community$list_extra$List_Extra_ops['!!'] = _elm_lang$core$Basics$flip(_elm_community$list_extra$List_Extra$getAt);
var _elm_community$list_extra$List_Extra$init = function () {
	var maybe = F2(
		function (d, f) {
			return function (_p92) {
				return A2(
					_elm_lang$core$Maybe$withDefault,
					d,
					A2(_elm_lang$core$Maybe$map, f, _p92));
			};
		});
	return A2(
		_elm_lang$core$List$foldr,
		function (x) {
			return function (_p93) {
				return _elm_lang$core$Maybe$Just(
					A3(
						maybe,
						{ctor: '[]'},
						F2(
							function (x, y) {
								return {ctor: '::', _0: x, _1: y};
							})(x),
						_p93));
			};
		},
		_elm_lang$core$Maybe$Nothing);
}();
var _elm_community$list_extra$List_Extra$last = _elm_community$list_extra$List_Extra$foldl1(
	_elm_lang$core$Basics$flip(_elm_lang$core$Basics$always));

//import Native.List //

var _elm_lang$core$Native_Array = function() {

// A RRB-Tree has two distinct data types.
// Leaf -> "height"  is always 0
//         "table"   is an array of elements
// Node -> "height"  is always greater than 0
//         "table"   is an array of child nodes
//         "lengths" is an array of accumulated lengths of the child nodes

// M is the maximal table size. 32 seems fast. E is the allowed increase
// of search steps when concatting to find an index. Lower values will
// decrease balancing, but will increase search steps.
var M = 32;
var E = 2;

// An empty array.
var empty = {
	ctor: '_Array',
	height: 0,
	table: []
};


function get(i, array)
{
	if (i < 0 || i >= length(array))
	{
		throw new Error(
			'Index ' + i + ' is out of range. Check the length of ' +
			'your array first or use getMaybe or getWithDefault.');
	}
	return unsafeGet(i, array);
}


function unsafeGet(i, array)
{
	for (var x = array.height; x > 0; x--)
	{
		var slot = i >> (x * 5);
		while (array.lengths[slot] <= i)
		{
			slot++;
		}
		if (slot > 0)
		{
			i -= array.lengths[slot - 1];
		}
		array = array.table[slot];
	}
	return array.table[i];
}


// Sets the value at the index i. Only the nodes leading to i will get
// copied and updated.
function set(i, item, array)
{
	if (i < 0 || length(array) <= i)
	{
		return array;
	}
	return unsafeSet(i, item, array);
}


function unsafeSet(i, item, array)
{
	array = nodeCopy(array);

	if (array.height === 0)
	{
		array.table[i] = item;
	}
	else
	{
		var slot = getSlot(i, array);
		if (slot > 0)
		{
			i -= array.lengths[slot - 1];
		}
		array.table[slot] = unsafeSet(i, item, array.table[slot]);
	}
	return array;
}


function initialize(len, f)
{
	if (len <= 0)
	{
		return empty;
	}
	var h = Math.floor( Math.log(len) / Math.log(M) );
	return initialize_(f, h, 0, len);
}

function initialize_(f, h, from, to)
{
	if (h === 0)
	{
		var table = new Array((to - from) % (M + 1));
		for (var i = 0; i < table.length; i++)
		{
		  table[i] = f(from + i);
		}
		return {
			ctor: '_Array',
			height: 0,
			table: table
		};
	}

	var step = Math.pow(M, h);
	var table = new Array(Math.ceil((to - from) / step));
	var lengths = new Array(table.length);
	for (var i = 0; i < table.length; i++)
	{
		table[i] = initialize_(f, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
		lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
	}
	return {
		ctor: '_Array',
		height: h,
		table: table,
		lengths: lengths
	};
}

function fromList(list)
{
	if (list.ctor === '[]')
	{
		return empty;
	}

	// Allocate M sized blocks (table) and write list elements to it.
	var table = new Array(M);
	var nodes = [];
	var i = 0;

	while (list.ctor !== '[]')
	{
		table[i] = list._0;
		list = list._1;
		i++;

		// table is full, so we can push a leaf containing it into the
		// next node.
		if (i === M)
		{
			var leaf = {
				ctor: '_Array',
				height: 0,
				table: table
			};
			fromListPush(leaf, nodes);
			table = new Array(M);
			i = 0;
		}
	}

	// Maybe there is something left on the table.
	if (i > 0)
	{
		var leaf = {
			ctor: '_Array',
			height: 0,
			table: table.splice(0, i)
		};
		fromListPush(leaf, nodes);
	}

	// Go through all of the nodes and eventually push them into higher nodes.
	for (var h = 0; h < nodes.length - 1; h++)
	{
		if (nodes[h].table.length > 0)
		{
			fromListPush(nodes[h], nodes);
		}
	}

	var head = nodes[nodes.length - 1];
	if (head.height > 0 && head.table.length === 1)
	{
		return head.table[0];
	}
	else
	{
		return head;
	}
}

// Push a node into a higher node as a child.
function fromListPush(toPush, nodes)
{
	var h = toPush.height;

	// Maybe the node on this height does not exist.
	if (nodes.length === h)
	{
		var node = {
			ctor: '_Array',
			height: h + 1,
			table: [],
			lengths: []
		};
		nodes.push(node);
	}

	nodes[h].table.push(toPush);
	var len = length(toPush);
	if (nodes[h].lengths.length > 0)
	{
		len += nodes[h].lengths[nodes[h].lengths.length - 1];
	}
	nodes[h].lengths.push(len);

	if (nodes[h].table.length === M)
	{
		fromListPush(nodes[h], nodes);
		nodes[h] = {
			ctor: '_Array',
			height: h + 1,
			table: [],
			lengths: []
		};
	}
}

// Pushes an item via push_ to the bottom right of a tree.
function push(item, a)
{
	var pushed = push_(item, a);
	if (pushed !== null)
	{
		return pushed;
	}

	var newTree = create(item, a.height);
	return siblise(a, newTree);
}

// Recursively tries to push an item to the bottom-right most
// tree possible. If there is no space left for the item,
// null will be returned.
function push_(item, a)
{
	// Handle resursion stop at leaf level.
	if (a.height === 0)
	{
		if (a.table.length < M)
		{
			var newA = {
				ctor: '_Array',
				height: 0,
				table: a.table.slice()
			};
			newA.table.push(item);
			return newA;
		}
		else
		{
		  return null;
		}
	}

	// Recursively push
	var pushed = push_(item, botRight(a));

	// There was space in the bottom right tree, so the slot will
	// be updated.
	if (pushed !== null)
	{
		var newA = nodeCopy(a);
		newA.table[newA.table.length - 1] = pushed;
		newA.lengths[newA.lengths.length - 1]++;
		return newA;
	}

	// When there was no space left, check if there is space left
	// for a new slot with a tree which contains only the item
	// at the bottom.
	if (a.table.length < M)
	{
		var newSlot = create(item, a.height - 1);
		var newA = nodeCopy(a);
		newA.table.push(newSlot);
		newA.lengths.push(newA.lengths[newA.lengths.length - 1] + length(newSlot));
		return newA;
	}
	else
	{
		return null;
	}
}

// Converts an array into a list of elements.
function toList(a)
{
	return toList_(_elm_lang$core$Native_List.Nil, a);
}

function toList_(list, a)
{
	for (var i = a.table.length - 1; i >= 0; i--)
	{
		list =
			a.height === 0
				? _elm_lang$core$Native_List.Cons(a.table[i], list)
				: toList_(list, a.table[i]);
	}
	return list;
}

// Maps a function over the elements of an array.
function map(f, a)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: new Array(a.table.length)
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths;
	}
	for (var i = 0; i < a.table.length; i++)
	{
		newA.table[i] =
			a.height === 0
				? f(a.table[i])
				: map(f, a.table[i]);
	}
	return newA;
}

// Maps a function over the elements with their index as first argument.
function indexedMap(f, a)
{
	return indexedMap_(f, a, 0);
}

function indexedMap_(f, a, from)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: new Array(a.table.length)
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths;
	}
	for (var i = 0; i < a.table.length; i++)
	{
		newA.table[i] =
			a.height === 0
				? A2(f, from + i, a.table[i])
				: indexedMap_(f, a.table[i], i == 0 ? from : from + a.lengths[i - 1]);
	}
	return newA;
}

function foldl(f, b, a)
{
	if (a.height === 0)
	{
		for (var i = 0; i < a.table.length; i++)
		{
			b = A2(f, a.table[i], b);
		}
	}
	else
	{
		for (var i = 0; i < a.table.length; i++)
		{
			b = foldl(f, b, a.table[i]);
		}
	}
	return b;
}

function foldr(f, b, a)
{
	if (a.height === 0)
	{
		for (var i = a.table.length; i--; )
		{
			b = A2(f, a.table[i], b);
		}
	}
	else
	{
		for (var i = a.table.length; i--; )
		{
			b = foldr(f, b, a.table[i]);
		}
	}
	return b;
}

// TODO: currently, it slices the right, then the left. This can be
// optimized.
function slice(from, to, a)
{
	if (from < 0)
	{
		from += length(a);
	}
	if (to < 0)
	{
		to += length(a);
	}
	return sliceLeft(from, sliceRight(to, a));
}

function sliceRight(to, a)
{
	if (to === length(a))
	{
		return a;
	}

	// Handle leaf level.
	if (a.height === 0)
	{
		var newA = { ctor:'_Array', height:0 };
		newA.table = a.table.slice(0, to);
		return newA;
	}

	// Slice the right recursively.
	var right = getSlot(to, a);
	var sliced = sliceRight(to - (right > 0 ? a.lengths[right - 1] : 0), a.table[right]);

	// Maybe the a node is not even needed, as sliced contains the whole slice.
	if (right === 0)
	{
		return sliced;
	}

	// Create new node.
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice(0, right),
		lengths: a.lengths.slice(0, right)
	};
	if (sliced.table.length > 0)
	{
		newA.table[right] = sliced;
		newA.lengths[right] = length(sliced) + (right > 0 ? newA.lengths[right - 1] : 0);
	}
	return newA;
}

function sliceLeft(from, a)
{
	if (from === 0)
	{
		return a;
	}

	// Handle leaf level.
	if (a.height === 0)
	{
		var newA = { ctor:'_Array', height:0 };
		newA.table = a.table.slice(from, a.table.length + 1);
		return newA;
	}

	// Slice the left recursively.
	var left = getSlot(from, a);
	var sliced = sliceLeft(from - (left > 0 ? a.lengths[left - 1] : 0), a.table[left]);

	// Maybe the a node is not even needed, as sliced contains the whole slice.
	if (left === a.table.length - 1)
	{
		return sliced;
	}

	// Create new node.
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice(left, a.table.length + 1),
		lengths: new Array(a.table.length - left)
	};
	newA.table[0] = sliced;
	var len = 0;
	for (var i = 0; i < newA.table.length; i++)
	{
		len += length(newA.table[i]);
		newA.lengths[i] = len;
	}

	return newA;
}

// Appends two trees.
function append(a,b)
{
	if (a.table.length === 0)
	{
		return b;
	}
	if (b.table.length === 0)
	{
		return a;
	}

	var c = append_(a, b);

	// Check if both nodes can be crunshed together.
	if (c[0].table.length + c[1].table.length <= M)
	{
		if (c[0].table.length === 0)
		{
			return c[1];
		}
		if (c[1].table.length === 0)
		{
			return c[0];
		}

		// Adjust .table and .lengths
		c[0].table = c[0].table.concat(c[1].table);
		if (c[0].height > 0)
		{
			var len = length(c[0]);
			for (var i = 0; i < c[1].lengths.length; i++)
			{
				c[1].lengths[i] += len;
			}
			c[0].lengths = c[0].lengths.concat(c[1].lengths);
		}

		return c[0];
	}

	if (c[0].height > 0)
	{
		var toRemove = calcToRemove(a, b);
		if (toRemove > E)
		{
			c = shuffle(c[0], c[1], toRemove);
		}
	}

	return siblise(c[0], c[1]);
}

// Returns an array of two nodes; right and left. One node _may_ be empty.
function append_(a, b)
{
	if (a.height === 0 && b.height === 0)
	{
		return [a, b];
	}

	if (a.height !== 1 || b.height !== 1)
	{
		if (a.height === b.height)
		{
			a = nodeCopy(a);
			b = nodeCopy(b);
			var appended = append_(botRight(a), botLeft(b));

			insertRight(a, appended[1]);
			insertLeft(b, appended[0]);
		}
		else if (a.height > b.height)
		{
			a = nodeCopy(a);
			var appended = append_(botRight(a), b);

			insertRight(a, appended[0]);
			b = parentise(appended[1], appended[1].height + 1);
		}
		else
		{
			b = nodeCopy(b);
			var appended = append_(a, botLeft(b));

			var left = appended[0].table.length === 0 ? 0 : 1;
			var right = left === 0 ? 1 : 0;
			insertLeft(b, appended[left]);
			a = parentise(appended[right], appended[right].height + 1);
		}
	}

	// Check if balancing is needed and return based on that.
	if (a.table.length === 0 || b.table.length === 0)
	{
		return [a, b];
	}

	var toRemove = calcToRemove(a, b);
	if (toRemove <= E)
	{
		return [a, b];
	}
	return shuffle(a, b, toRemove);
}

// Helperfunctions for append_. Replaces a child node at the side of the parent.
function insertRight(parent, node)
{
	var index = parent.table.length - 1;
	parent.table[index] = node;
	parent.lengths[index] = length(node);
	parent.lengths[index] += index > 0 ? parent.lengths[index - 1] : 0;
}

function insertLeft(parent, node)
{
	if (node.table.length > 0)
	{
		parent.table[0] = node;
		parent.lengths[0] = length(node);

		var len = length(parent.table[0]);
		for (var i = 1; i < parent.lengths.length; i++)
		{
			len += length(parent.table[i]);
			parent.lengths[i] = len;
		}
	}
	else
	{
		parent.table.shift();
		for (var i = 1; i < parent.lengths.length; i++)
		{
			parent.lengths[i] = parent.lengths[i] - parent.lengths[0];
		}
		parent.lengths.shift();
	}
}

// Returns the extra search steps for E. Refer to the paper.
function calcToRemove(a, b)
{
	var subLengths = 0;
	for (var i = 0; i < a.table.length; i++)
	{
		subLengths += a.table[i].table.length;
	}
	for (var i = 0; i < b.table.length; i++)
	{
		subLengths += b.table[i].table.length;
	}

	var toRemove = a.table.length + b.table.length;
	return toRemove - (Math.floor((subLengths - 1) / M) + 1);
}

// get2, set2 and saveSlot are helpers for accessing elements over two arrays.
function get2(a, b, index)
{
	return index < a.length
		? a[index]
		: b[index - a.length];
}

function set2(a, b, index, value)
{
	if (index < a.length)
	{
		a[index] = value;
	}
	else
	{
		b[index - a.length] = value;
	}
}

function saveSlot(a, b, index, slot)
{
	set2(a.table, b.table, index, slot);

	var l = (index === 0 || index === a.lengths.length)
		? 0
		: get2(a.lengths, a.lengths, index - 1);

	set2(a.lengths, b.lengths, index, l + length(slot));
}

// Creates a node or leaf with a given length at their arrays for perfomance.
// Is only used by shuffle.
function createNode(h, length)
{
	if (length < 0)
	{
		length = 0;
	}
	var a = {
		ctor: '_Array',
		height: h,
		table: new Array(length)
	};
	if (h > 0)
	{
		a.lengths = new Array(length);
	}
	return a;
}

// Returns an array of two balanced nodes.
function shuffle(a, b, toRemove)
{
	var newA = createNode(a.height, Math.min(M, a.table.length + b.table.length - toRemove));
	var newB = createNode(a.height, newA.table.length - (a.table.length + b.table.length - toRemove));

	// Skip the slots with size M. More precise: copy the slot references
	// to the new node
	var read = 0;
	while (get2(a.table, b.table, read).table.length % M === 0)
	{
		set2(newA.table, newB.table, read, get2(a.table, b.table, read));
		set2(newA.lengths, newB.lengths, read, get2(a.lengths, b.lengths, read));
		read++;
	}

	// Pulling items from left to right, caching in a slot before writing
	// it into the new nodes.
	var write = read;
	var slot = new createNode(a.height - 1, 0);
	var from = 0;

	// If the current slot is still containing data, then there will be at
	// least one more write, so we do not break this loop yet.
	while (read - write - (slot.table.length > 0 ? 1 : 0) < toRemove)
	{
		// Find out the max possible items for copying.
		var source = get2(a.table, b.table, read);
		var to = Math.min(M - slot.table.length, source.table.length);

		// Copy and adjust size table.
		slot.table = slot.table.concat(source.table.slice(from, to));
		if (slot.height > 0)
		{
			var len = slot.lengths.length;
			for (var i = len; i < len + to - from; i++)
			{
				slot.lengths[i] = length(slot.table[i]);
				slot.lengths[i] += (i > 0 ? slot.lengths[i - 1] : 0);
			}
		}

		from += to;

		// Only proceed to next slots[i] if the current one was
		// fully copied.
		if (source.table.length <= to)
		{
			read++; from = 0;
		}

		// Only create a new slot if the current one is filled up.
		if (slot.table.length === M)
		{
			saveSlot(newA, newB, write, slot);
			slot = createNode(a.height - 1, 0);
			write++;
		}
	}

	// Cleanup after the loop. Copy the last slot into the new nodes.
	if (slot.table.length > 0)
	{
		saveSlot(newA, newB, write, slot);
		write++;
	}

	// Shift the untouched slots to the left
	while (read < a.table.length + b.table.length )
	{
		saveSlot(newA, newB, write, get2(a.table, b.table, read));
		read++;
		write++;
	}

	return [newA, newB];
}

// Navigation functions
function botRight(a)
{
	return a.table[a.table.length - 1];
}
function botLeft(a)
{
	return a.table[0];
}

// Copies a node for updating. Note that you should not use this if
// only updating only one of "table" or "lengths" for performance reasons.
function nodeCopy(a)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice()
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths.slice();
	}
	return newA;
}

// Returns how many items are in the tree.
function length(array)
{
	if (array.height === 0)
	{
		return array.table.length;
	}
	else
	{
		return array.lengths[array.lengths.length - 1];
	}
}

// Calculates in which slot of "table" the item probably is, then
// find the exact slot via forward searching in  "lengths". Returns the index.
function getSlot(i, a)
{
	var slot = i >> (5 * a.height);
	while (a.lengths[slot] <= i)
	{
		slot++;
	}
	return slot;
}

// Recursively creates a tree with a given height containing
// only the given item.
function create(item, h)
{
	if (h === 0)
	{
		return {
			ctor: '_Array',
			height: 0,
			table: [item]
		};
	}
	return {
		ctor: '_Array',
		height: h,
		table: [create(item, h - 1)],
		lengths: [1]
	};
}

// Recursively creates a tree that contains the given tree.
function parentise(tree, h)
{
	if (h === tree.height)
	{
		return tree;
	}

	return {
		ctor: '_Array',
		height: h,
		table: [parentise(tree, h - 1)],
		lengths: [length(tree)]
	};
}

// Emphasizes blood brotherhood beneath two trees.
function siblise(a, b)
{
	return {
		ctor: '_Array',
		height: a.height + 1,
		table: [a, b],
		lengths: [length(a), length(a) + length(b)]
	};
}

function toJSArray(a)
{
	var jsArray = new Array(length(a));
	toJSArray_(jsArray, 0, a);
	return jsArray;
}

function toJSArray_(jsArray, i, a)
{
	for (var t = 0; t < a.table.length; t++)
	{
		if (a.height === 0)
		{
			jsArray[i + t] = a.table[t];
		}
		else
		{
			var inc = t === 0 ? 0 : a.lengths[t - 1];
			toJSArray_(jsArray, i + inc, a.table[t]);
		}
	}
}

function fromJSArray(jsArray)
{
	if (jsArray.length === 0)
	{
		return empty;
	}
	var h = Math.floor(Math.log(jsArray.length) / Math.log(M));
	return fromJSArray_(jsArray, h, 0, jsArray.length);
}

function fromJSArray_(jsArray, h, from, to)
{
	if (h === 0)
	{
		return {
			ctor: '_Array',
			height: 0,
			table: jsArray.slice(from, to)
		};
	}

	var step = Math.pow(M, h);
	var table = new Array(Math.ceil((to - from) / step));
	var lengths = new Array(table.length);
	for (var i = 0; i < table.length; i++)
	{
		table[i] = fromJSArray_(jsArray, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
		lengths[i] = length(table[i]) + (i > 0 ? lengths[i - 1] : 0);
	}
	return {
		ctor: '_Array',
		height: h,
		table: table,
		lengths: lengths
	};
}

return {
	empty: empty,
	fromList: fromList,
	toList: toList,
	initialize: F2(initialize),
	append: F2(append),
	push: F2(push),
	slice: F3(slice),
	get: F2(get),
	set: F3(set),
	map: F2(map),
	indexedMap: F2(indexedMap),
	foldl: F3(foldl),
	foldr: F3(foldr),
	length: length,

	toJSArray: toJSArray,
	fromJSArray: fromJSArray
};

}();
var _elm_lang$core$Array$append = _elm_lang$core$Native_Array.append;
var _elm_lang$core$Array$length = _elm_lang$core$Native_Array.length;
var _elm_lang$core$Array$isEmpty = function (array) {
	return _elm_lang$core$Native_Utils.eq(
		_elm_lang$core$Array$length(array),
		0);
};
var _elm_lang$core$Array$slice = _elm_lang$core$Native_Array.slice;
var _elm_lang$core$Array$set = _elm_lang$core$Native_Array.set;
var _elm_lang$core$Array$get = F2(
	function (i, array) {
		return ((_elm_lang$core$Native_Utils.cmp(0, i) < 1) && (_elm_lang$core$Native_Utils.cmp(
			i,
			_elm_lang$core$Native_Array.length(array)) < 0)) ? _elm_lang$core$Maybe$Just(
			A2(_elm_lang$core$Native_Array.get, i, array)) : _elm_lang$core$Maybe$Nothing;
	});
var _elm_lang$core$Array$push = _elm_lang$core$Native_Array.push;
var _elm_lang$core$Array$empty = _elm_lang$core$Native_Array.empty;
var _elm_lang$core$Array$filter = F2(
	function (isOkay, arr) {
		var update = F2(
			function (x, xs) {
				return isOkay(x) ? A2(_elm_lang$core$Native_Array.push, x, xs) : xs;
			});
		return A3(_elm_lang$core$Native_Array.foldl, update, _elm_lang$core$Native_Array.empty, arr);
	});
var _elm_lang$core$Array$foldr = _elm_lang$core$Native_Array.foldr;
var _elm_lang$core$Array$foldl = _elm_lang$core$Native_Array.foldl;
var _elm_lang$core$Array$indexedMap = _elm_lang$core$Native_Array.indexedMap;
var _elm_lang$core$Array$map = _elm_lang$core$Native_Array.map;
var _elm_lang$core$Array$toIndexedList = function (array) {
	return A3(
		_elm_lang$core$List$map2,
		F2(
			function (v0, v1) {
				return {ctor: '_Tuple2', _0: v0, _1: v1};
			}),
		A2(
			_elm_lang$core$List$range,
			0,
			_elm_lang$core$Native_Array.length(array) - 1),
		_elm_lang$core$Native_Array.toList(array));
};
var _elm_lang$core$Array$toList = _elm_lang$core$Native_Array.toList;
var _elm_lang$core$Array$fromList = _elm_lang$core$Native_Array.fromList;
var _elm_lang$core$Array$initialize = _elm_lang$core$Native_Array.initialize;
var _elm_lang$core$Array$repeat = F2(
	function (n, e) {
		return A2(
			_elm_lang$core$Array$initialize,
			n,
			_elm_lang$core$Basics$always(e));
	});
var _elm_lang$core$Array$Array = {ctor: 'Array'};

var _elm_lang$core$Task$onError = _elm_lang$core$Native_Scheduler.onError;
var _elm_lang$core$Task$andThen = _elm_lang$core$Native_Scheduler.andThen;
var _elm_lang$core$Task$spawnCmd = F2(
	function (router, _p0) {
		var _p1 = _p0;
		return _elm_lang$core$Native_Scheduler.spawn(
			A2(
				_elm_lang$core$Task$andThen,
				_elm_lang$core$Platform$sendToApp(router),
				_p1._0));
	});
var _elm_lang$core$Task$fail = _elm_lang$core$Native_Scheduler.fail;
var _elm_lang$core$Task$mapError = F2(
	function (convert, task) {
		return A2(
			_elm_lang$core$Task$onError,
			function (_p2) {
				return _elm_lang$core$Task$fail(
					convert(_p2));
			},
			task);
	});
var _elm_lang$core$Task$succeed = _elm_lang$core$Native_Scheduler.succeed;
var _elm_lang$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return _elm_lang$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var _elm_lang$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return _elm_lang$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map3 = F4(
	function (func, taskA, taskB, taskC) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return _elm_lang$core$Task$succeed(
									A3(func, a, b, c));
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map4 = F5(
	function (func, taskA, taskB, taskC, taskD) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return A2(
									_elm_lang$core$Task$andThen,
									function (d) {
										return _elm_lang$core$Task$succeed(
											A4(func, a, b, c, d));
									},
									taskD);
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map5 = F6(
	function (func, taskA, taskB, taskC, taskD, taskE) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return A2(
									_elm_lang$core$Task$andThen,
									function (d) {
										return A2(
											_elm_lang$core$Task$andThen,
											function (e) {
												return _elm_lang$core$Task$succeed(
													A5(func, a, b, c, d, e));
											},
											taskE);
									},
									taskD);
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$sequence = function (tasks) {
	var _p3 = tasks;
	if (_p3.ctor === '[]') {
		return _elm_lang$core$Task$succeed(
			{ctor: '[]'});
	} else {
		return A3(
			_elm_lang$core$Task$map2,
			F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				}),
			_p3._0,
			_elm_lang$core$Task$sequence(_p3._1));
	}
};
var _elm_lang$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			_elm_lang$core$Task$map,
			function (_p4) {
				return {ctor: '_Tuple0'};
			},
			_elm_lang$core$Task$sequence(
				A2(
					_elm_lang$core$List$map,
					_elm_lang$core$Task$spawnCmd(router),
					commands)));
	});
var _elm_lang$core$Task$init = _elm_lang$core$Task$succeed(
	{ctor: '_Tuple0'});
var _elm_lang$core$Task$onSelfMsg = F3(
	function (_p7, _p6, _p5) {
		return _elm_lang$core$Task$succeed(
			{ctor: '_Tuple0'});
	});
var _elm_lang$core$Task$command = _elm_lang$core$Native_Platform.leaf('Task');
var _elm_lang$core$Task$Perform = function (a) {
	return {ctor: 'Perform', _0: a};
};
var _elm_lang$core$Task$perform = F2(
	function (toMessage, task) {
		return _elm_lang$core$Task$command(
			_elm_lang$core$Task$Perform(
				A2(_elm_lang$core$Task$map, toMessage, task)));
	});
var _elm_lang$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return _elm_lang$core$Task$command(
			_elm_lang$core$Task$Perform(
				A2(
					_elm_lang$core$Task$onError,
					function (_p8) {
						return _elm_lang$core$Task$succeed(
							resultToMessage(
								_elm_lang$core$Result$Err(_p8)));
					},
					A2(
						_elm_lang$core$Task$andThen,
						function (_p9) {
							return _elm_lang$core$Task$succeed(
								resultToMessage(
									_elm_lang$core$Result$Ok(_p9)));
						},
						task))));
	});
var _elm_lang$core$Task$cmdMap = F2(
	function (tagger, _p10) {
		var _p11 = _p10;
		return _elm_lang$core$Task$Perform(
			A2(_elm_lang$core$Task$map, tagger, _p11._0));
	});
_elm_lang$core$Native_Platform.effectManagers['Task'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Task$init, onEffects: _elm_lang$core$Task$onEffects, onSelfMsg: _elm_lang$core$Task$onSelfMsg, tag: 'cmd', cmdMap: _elm_lang$core$Task$cmdMap};

//import Native.Scheduler //

var _elm_lang$core$Native_Time = function() {

var now = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
{
	callback(_elm_lang$core$Native_Scheduler.succeed(Date.now()));
});

function setInterval_(interval, task)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var id = setInterval(function() {
			_elm_lang$core$Native_Scheduler.rawSpawn(task);
		}, interval);

		return function() { clearInterval(id); };
	});
}

return {
	now: now,
	setInterval_: F2(setInterval_)
};

}();
var _elm_lang$core$Time$setInterval = _elm_lang$core$Native_Time.setInterval_;
var _elm_lang$core$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		var _p0 = intervals;
		if (_p0.ctor === '[]') {
			return _elm_lang$core$Task$succeed(processes);
		} else {
			var _p1 = _p0._0;
			var spawnRest = function (id) {
				return A3(
					_elm_lang$core$Time$spawnHelp,
					router,
					_p0._1,
					A3(_elm_lang$core$Dict$insert, _p1, id, processes));
			};
			var spawnTimer = _elm_lang$core$Native_Scheduler.spawn(
				A2(
					_elm_lang$core$Time$setInterval,
					_p1,
					A2(_elm_lang$core$Platform$sendToSelf, router, _p1)));
			return A2(_elm_lang$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var _elm_lang$core$Time$addMySub = F2(
	function (_p2, state) {
		var _p3 = _p2;
		var _p6 = _p3._1;
		var _p5 = _p3._0;
		var _p4 = A2(_elm_lang$core$Dict$get, _p5, state);
		if (_p4.ctor === 'Nothing') {
			return A3(
				_elm_lang$core$Dict$insert,
				_p5,
				{
					ctor: '::',
					_0: _p6,
					_1: {ctor: '[]'}
				},
				state);
		} else {
			return A3(
				_elm_lang$core$Dict$insert,
				_p5,
				{ctor: '::', _0: _p6, _1: _p4._0},
				state);
		}
	});
var _elm_lang$core$Time$inMilliseconds = function (t) {
	return t;
};
var _elm_lang$core$Time$millisecond = 1;
var _elm_lang$core$Time$second = 1000 * _elm_lang$core$Time$millisecond;
var _elm_lang$core$Time$minute = 60 * _elm_lang$core$Time$second;
var _elm_lang$core$Time$hour = 60 * _elm_lang$core$Time$minute;
var _elm_lang$core$Time$inHours = function (t) {
	return t / _elm_lang$core$Time$hour;
};
var _elm_lang$core$Time$inMinutes = function (t) {
	return t / _elm_lang$core$Time$minute;
};
var _elm_lang$core$Time$inSeconds = function (t) {
	return t / _elm_lang$core$Time$second;
};
var _elm_lang$core$Time$now = _elm_lang$core$Native_Time.now;
var _elm_lang$core$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _p7 = A2(_elm_lang$core$Dict$get, interval, state.taggers);
		if (_p7.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var tellTaggers = function (time) {
				return _elm_lang$core$Task$sequence(
					A2(
						_elm_lang$core$List$map,
						function (tagger) {
							return A2(
								_elm_lang$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						_p7._0));
			};
			return A2(
				_elm_lang$core$Task$andThen,
				function (_p8) {
					return _elm_lang$core$Task$succeed(state);
				},
				A2(_elm_lang$core$Task$andThen, tellTaggers, _elm_lang$core$Time$now));
		}
	});
var _elm_lang$core$Time$subscription = _elm_lang$core$Native_Platform.leaf('Time');
var _elm_lang$core$Time$State = F2(
	function (a, b) {
		return {taggers: a, processes: b};
	});
var _elm_lang$core$Time$init = _elm_lang$core$Task$succeed(
	A2(_elm_lang$core$Time$State, _elm_lang$core$Dict$empty, _elm_lang$core$Dict$empty));
var _elm_lang$core$Time$onEffects = F3(
	function (router, subs, _p9) {
		var _p10 = _p9;
		var rightStep = F3(
			function (_p12, id, _p11) {
				var _p13 = _p11;
				return {
					ctor: '_Tuple3',
					_0: _p13._0,
					_1: _p13._1,
					_2: A2(
						_elm_lang$core$Task$andThen,
						function (_p14) {
							return _p13._2;
						},
						_elm_lang$core$Native_Scheduler.kill(id))
				};
			});
		var bothStep = F4(
			function (interval, taggers, id, _p15) {
				var _p16 = _p15;
				return {
					ctor: '_Tuple3',
					_0: _p16._0,
					_1: A3(_elm_lang$core$Dict$insert, interval, id, _p16._1),
					_2: _p16._2
				};
			});
		var leftStep = F3(
			function (interval, taggers, _p17) {
				var _p18 = _p17;
				return {
					ctor: '_Tuple3',
					_0: {ctor: '::', _0: interval, _1: _p18._0},
					_1: _p18._1,
					_2: _p18._2
				};
			});
		var newTaggers = A3(_elm_lang$core$List$foldl, _elm_lang$core$Time$addMySub, _elm_lang$core$Dict$empty, subs);
		var _p19 = A6(
			_elm_lang$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			_p10.processes,
			{
				ctor: '_Tuple3',
				_0: {ctor: '[]'},
				_1: _elm_lang$core$Dict$empty,
				_2: _elm_lang$core$Task$succeed(
					{ctor: '_Tuple0'})
			});
		var spawnList = _p19._0;
		var existingDict = _p19._1;
		var killTask = _p19._2;
		return A2(
			_elm_lang$core$Task$andThen,
			function (newProcesses) {
				return _elm_lang$core$Task$succeed(
					A2(_elm_lang$core$Time$State, newTaggers, newProcesses));
			},
			A2(
				_elm_lang$core$Task$andThen,
				function (_p20) {
					return A3(_elm_lang$core$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var _elm_lang$core$Time$Every = F2(
	function (a, b) {
		return {ctor: 'Every', _0: a, _1: b};
	});
var _elm_lang$core$Time$every = F2(
	function (interval, tagger) {
		return _elm_lang$core$Time$subscription(
			A2(_elm_lang$core$Time$Every, interval, tagger));
	});
var _elm_lang$core$Time$subMap = F2(
	function (f, _p21) {
		var _p22 = _p21;
		return A2(
			_elm_lang$core$Time$Every,
			_p22._0,
			function (_p23) {
				return f(
					_p22._1(_p23));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Time'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Time$init, onEffects: _elm_lang$core$Time$onEffects, onSelfMsg: _elm_lang$core$Time$onSelfMsg, tag: 'sub', subMap: _elm_lang$core$Time$subMap};

var _elm_lang$core$Random$onSelfMsg = F3(
	function (_p1, _p0, seed) {
		return _elm_lang$core$Task$succeed(seed);
	});
var _elm_lang$core$Random$magicNum8 = 2147483562;
var _elm_lang$core$Random$range = function (_p2) {
	return {ctor: '_Tuple2', _0: 0, _1: _elm_lang$core$Random$magicNum8};
};
var _elm_lang$core$Random$magicNum7 = 2147483399;
var _elm_lang$core$Random$magicNum6 = 2147483563;
var _elm_lang$core$Random$magicNum5 = 3791;
var _elm_lang$core$Random$magicNum4 = 40692;
var _elm_lang$core$Random$magicNum3 = 52774;
var _elm_lang$core$Random$magicNum2 = 12211;
var _elm_lang$core$Random$magicNum1 = 53668;
var _elm_lang$core$Random$magicNum0 = 40014;
var _elm_lang$core$Random$step = F2(
	function (_p3, seed) {
		var _p4 = _p3;
		return _p4._0(seed);
	});
var _elm_lang$core$Random$onEffects = F3(
	function (router, commands, seed) {
		var _p5 = commands;
		if (_p5.ctor === '[]') {
			return _elm_lang$core$Task$succeed(seed);
		} else {
			var _p6 = A2(_elm_lang$core$Random$step, _p5._0._0, seed);
			var value = _p6._0;
			var newSeed = _p6._1;
			return A2(
				_elm_lang$core$Task$andThen,
				function (_p7) {
					return A3(_elm_lang$core$Random$onEffects, router, _p5._1, newSeed);
				},
				A2(_elm_lang$core$Platform$sendToApp, router, value));
		}
	});
var _elm_lang$core$Random$listHelp = F4(
	function (list, n, generate, seed) {
		listHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 1) < 0) {
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$List$reverse(list),
					_1: seed
				};
			} else {
				var _p8 = generate(seed);
				var value = _p8._0;
				var newSeed = _p8._1;
				var _v2 = {ctor: '::', _0: value, _1: list},
					_v3 = n - 1,
					_v4 = generate,
					_v5 = newSeed;
				list = _v2;
				n = _v3;
				generate = _v4;
				seed = _v5;
				continue listHelp;
			}
		}
	});
var _elm_lang$core$Random$minInt = -2147483648;
var _elm_lang$core$Random$maxInt = 2147483647;
var _elm_lang$core$Random$iLogBase = F2(
	function (b, i) {
		return (_elm_lang$core$Native_Utils.cmp(i, b) < 0) ? 1 : (1 + A2(_elm_lang$core$Random$iLogBase, b, (i / b) | 0));
	});
var _elm_lang$core$Random$command = _elm_lang$core$Native_Platform.leaf('Random');
var _elm_lang$core$Random$Generator = function (a) {
	return {ctor: 'Generator', _0: a};
};
var _elm_lang$core$Random$list = F2(
	function (n, _p9) {
		var _p10 = _p9;
		return _elm_lang$core$Random$Generator(
			function (seed) {
				return A4(
					_elm_lang$core$Random$listHelp,
					{ctor: '[]'},
					n,
					_p10._0,
					seed);
			});
	});
var _elm_lang$core$Random$map = F2(
	function (func, _p11) {
		var _p12 = _p11;
		return _elm_lang$core$Random$Generator(
			function (seed0) {
				var _p13 = _p12._0(seed0);
				var a = _p13._0;
				var seed1 = _p13._1;
				return {
					ctor: '_Tuple2',
					_0: func(a),
					_1: seed1
				};
			});
	});
var _elm_lang$core$Random$map2 = F3(
	function (func, _p15, _p14) {
		var _p16 = _p15;
		var _p17 = _p14;
		return _elm_lang$core$Random$Generator(
			function (seed0) {
				var _p18 = _p16._0(seed0);
				var a = _p18._0;
				var seed1 = _p18._1;
				var _p19 = _p17._0(seed1);
				var b = _p19._0;
				var seed2 = _p19._1;
				return {
					ctor: '_Tuple2',
					_0: A2(func, a, b),
					_1: seed2
				};
			});
	});
var _elm_lang$core$Random$pair = F2(
	function (genA, genB) {
		return A3(
			_elm_lang$core$Random$map2,
			F2(
				function (v0, v1) {
					return {ctor: '_Tuple2', _0: v0, _1: v1};
				}),
			genA,
			genB);
	});
var _elm_lang$core$Random$map3 = F4(
	function (func, _p22, _p21, _p20) {
		var _p23 = _p22;
		var _p24 = _p21;
		var _p25 = _p20;
		return _elm_lang$core$Random$Generator(
			function (seed0) {
				var _p26 = _p23._0(seed0);
				var a = _p26._0;
				var seed1 = _p26._1;
				var _p27 = _p24._0(seed1);
				var b = _p27._0;
				var seed2 = _p27._1;
				var _p28 = _p25._0(seed2);
				var c = _p28._0;
				var seed3 = _p28._1;
				return {
					ctor: '_Tuple2',
					_0: A3(func, a, b, c),
					_1: seed3
				};
			});
	});
var _elm_lang$core$Random$map4 = F5(
	function (func, _p32, _p31, _p30, _p29) {
		var _p33 = _p32;
		var _p34 = _p31;
		var _p35 = _p30;
		var _p36 = _p29;
		return _elm_lang$core$Random$Generator(
			function (seed0) {
				var _p37 = _p33._0(seed0);
				var a = _p37._0;
				var seed1 = _p37._1;
				var _p38 = _p34._0(seed1);
				var b = _p38._0;
				var seed2 = _p38._1;
				var _p39 = _p35._0(seed2);
				var c = _p39._0;
				var seed3 = _p39._1;
				var _p40 = _p36._0(seed3);
				var d = _p40._0;
				var seed4 = _p40._1;
				return {
					ctor: '_Tuple2',
					_0: A4(func, a, b, c, d),
					_1: seed4
				};
			});
	});
var _elm_lang$core$Random$map5 = F6(
	function (func, _p45, _p44, _p43, _p42, _p41) {
		var _p46 = _p45;
		var _p47 = _p44;
		var _p48 = _p43;
		var _p49 = _p42;
		var _p50 = _p41;
		return _elm_lang$core$Random$Generator(
			function (seed0) {
				var _p51 = _p46._0(seed0);
				var a = _p51._0;
				var seed1 = _p51._1;
				var _p52 = _p47._0(seed1);
				var b = _p52._0;
				var seed2 = _p52._1;
				var _p53 = _p48._0(seed2);
				var c = _p53._0;
				var seed3 = _p53._1;
				var _p54 = _p49._0(seed3);
				var d = _p54._0;
				var seed4 = _p54._1;
				var _p55 = _p50._0(seed4);
				var e = _p55._0;
				var seed5 = _p55._1;
				return {
					ctor: '_Tuple2',
					_0: A5(func, a, b, c, d, e),
					_1: seed5
				};
			});
	});
var _elm_lang$core$Random$andThen = F2(
	function (callback, _p56) {
		var _p57 = _p56;
		return _elm_lang$core$Random$Generator(
			function (seed) {
				var _p58 = _p57._0(seed);
				var result = _p58._0;
				var newSeed = _p58._1;
				var _p59 = callback(result);
				var genB = _p59._0;
				return genB(newSeed);
			});
	});
var _elm_lang$core$Random$State = F2(
	function (a, b) {
		return {ctor: 'State', _0: a, _1: b};
	});
var _elm_lang$core$Random$initState = function (seed) {
	var s = A2(_elm_lang$core$Basics$max, seed, 0 - seed);
	var q = (s / (_elm_lang$core$Random$magicNum6 - 1)) | 0;
	var s2 = A2(_elm_lang$core$Basics_ops['%'], q, _elm_lang$core$Random$magicNum7 - 1);
	var s1 = A2(_elm_lang$core$Basics_ops['%'], s, _elm_lang$core$Random$magicNum6 - 1);
	return A2(_elm_lang$core$Random$State, s1 + 1, s2 + 1);
};
var _elm_lang$core$Random$next = function (_p60) {
	var _p61 = _p60;
	var _p63 = _p61._1;
	var _p62 = _p61._0;
	var k2 = (_p63 / _elm_lang$core$Random$magicNum3) | 0;
	var rawState2 = (_elm_lang$core$Random$magicNum4 * (_p63 - (k2 * _elm_lang$core$Random$magicNum3))) - (k2 * _elm_lang$core$Random$magicNum5);
	var newState2 = (_elm_lang$core$Native_Utils.cmp(rawState2, 0) < 0) ? (rawState2 + _elm_lang$core$Random$magicNum7) : rawState2;
	var k1 = (_p62 / _elm_lang$core$Random$magicNum1) | 0;
	var rawState1 = (_elm_lang$core$Random$magicNum0 * (_p62 - (k1 * _elm_lang$core$Random$magicNum1))) - (k1 * _elm_lang$core$Random$magicNum2);
	var newState1 = (_elm_lang$core$Native_Utils.cmp(rawState1, 0) < 0) ? (rawState1 + _elm_lang$core$Random$magicNum6) : rawState1;
	var z = newState1 - newState2;
	var newZ = (_elm_lang$core$Native_Utils.cmp(z, 1) < 0) ? (z + _elm_lang$core$Random$magicNum8) : z;
	return {
		ctor: '_Tuple2',
		_0: newZ,
		_1: A2(_elm_lang$core$Random$State, newState1, newState2)
	};
};
var _elm_lang$core$Random$split = function (_p64) {
	var _p65 = _p64;
	var _p68 = _p65._1;
	var _p67 = _p65._0;
	var _p66 = _elm_lang$core$Tuple$second(
		_elm_lang$core$Random$next(_p65));
	var t1 = _p66._0;
	var t2 = _p66._1;
	var new_s2 = _elm_lang$core$Native_Utils.eq(_p68, 1) ? (_elm_lang$core$Random$magicNum7 - 1) : (_p68 - 1);
	var new_s1 = _elm_lang$core$Native_Utils.eq(_p67, _elm_lang$core$Random$magicNum6 - 1) ? 1 : (_p67 + 1);
	return {
		ctor: '_Tuple2',
		_0: A2(_elm_lang$core$Random$State, new_s1, t2),
		_1: A2(_elm_lang$core$Random$State, t1, new_s2)
	};
};
var _elm_lang$core$Random$Seed = function (a) {
	return {ctor: 'Seed', _0: a};
};
var _elm_lang$core$Random$int = F2(
	function (a, b) {
		return _elm_lang$core$Random$Generator(
			function (_p69) {
				var _p70 = _p69;
				var _p75 = _p70._0;
				var base = 2147483561;
				var f = F3(
					function (n, acc, state) {
						f:
						while (true) {
							var _p71 = n;
							if (_p71 === 0) {
								return {ctor: '_Tuple2', _0: acc, _1: state};
							} else {
								var _p72 = _p75.next(state);
								var x = _p72._0;
								var nextState = _p72._1;
								var _v27 = n - 1,
									_v28 = x + (acc * base),
									_v29 = nextState;
								n = _v27;
								acc = _v28;
								state = _v29;
								continue f;
							}
						}
					});
				var _p73 = (_elm_lang$core$Native_Utils.cmp(a, b) < 0) ? {ctor: '_Tuple2', _0: a, _1: b} : {ctor: '_Tuple2', _0: b, _1: a};
				var lo = _p73._0;
				var hi = _p73._1;
				var k = (hi - lo) + 1;
				var n = A2(_elm_lang$core$Random$iLogBase, base, k);
				var _p74 = A3(f, n, 1, _p75.state);
				var v = _p74._0;
				var nextState = _p74._1;
				return {
					ctor: '_Tuple2',
					_0: lo + A2(_elm_lang$core$Basics_ops['%'], v, k),
					_1: _elm_lang$core$Random$Seed(
						_elm_lang$core$Native_Utils.update(
							_p75,
							{state: nextState}))
				};
			});
	});
var _elm_lang$core$Random$bool = A2(
	_elm_lang$core$Random$map,
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.eq(x, y);
		})(1),
	A2(_elm_lang$core$Random$int, 0, 1));
var _elm_lang$core$Random$float = F2(
	function (a, b) {
		return _elm_lang$core$Random$Generator(
			function (seed) {
				var _p76 = A2(
					_elm_lang$core$Random$step,
					A2(_elm_lang$core$Random$int, _elm_lang$core$Random$minInt, _elm_lang$core$Random$maxInt),
					seed);
				var number = _p76._0;
				var newSeed = _p76._1;
				var negativeOneToOne = _elm_lang$core$Basics$toFloat(number) / _elm_lang$core$Basics$toFloat(_elm_lang$core$Random$maxInt - _elm_lang$core$Random$minInt);
				var _p77 = (_elm_lang$core$Native_Utils.cmp(a, b) < 0) ? {ctor: '_Tuple2', _0: a, _1: b} : {ctor: '_Tuple2', _0: b, _1: a};
				var lo = _p77._0;
				var hi = _p77._1;
				var scaled = ((lo + hi) / 2) + ((hi - lo) * negativeOneToOne);
				return {ctor: '_Tuple2', _0: scaled, _1: newSeed};
			});
	});
var _elm_lang$core$Random$initialSeed = function (n) {
	return _elm_lang$core$Random$Seed(
		{
			state: _elm_lang$core$Random$initState(n),
			next: _elm_lang$core$Random$next,
			split: _elm_lang$core$Random$split,
			range: _elm_lang$core$Random$range
		});
};
var _elm_lang$core$Random$init = A2(
	_elm_lang$core$Task$andThen,
	function (t) {
		return _elm_lang$core$Task$succeed(
			_elm_lang$core$Random$initialSeed(
				_elm_lang$core$Basics$round(t)));
	},
	_elm_lang$core$Time$now);
var _elm_lang$core$Random$Generate = function (a) {
	return {ctor: 'Generate', _0: a};
};
var _elm_lang$core$Random$generate = F2(
	function (tagger, generator) {
		return _elm_lang$core$Random$command(
			_elm_lang$core$Random$Generate(
				A2(_elm_lang$core$Random$map, tagger, generator)));
	});
var _elm_lang$core$Random$cmdMap = F2(
	function (func, _p78) {
		var _p79 = _p78;
		return _elm_lang$core$Random$Generate(
			A2(_elm_lang$core$Random$map, func, _p79._0));
	});
_elm_lang$core$Native_Platform.effectManagers['Random'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Random$init, onEffects: _elm_lang$core$Random$onEffects, onSelfMsg: _elm_lang$core$Random$onSelfMsg, tag: 'cmd', cmdMap: _elm_lang$core$Random$cmdMap};

var _elm_community$random_extra$Random_Extra$andThen6 = F7(
	function (constructor, generatorA, generatorB, generatorC, generatorD, generatorE, generatorF) {
		return A2(
			_elm_lang$core$Random$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Random$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Random$andThen,
							function (c) {
								return A2(
									_elm_lang$core$Random$andThen,
									function (d) {
										return A2(
											_elm_lang$core$Random$andThen,
											function (e) {
												return A2(
													_elm_lang$core$Random$andThen,
													function (f) {
														return A6(constructor, a, b, c, d, e, f);
													},
													generatorF);
											},
											generatorE);
									},
									generatorD);
							},
							generatorC);
					},
					generatorB);
			},
			generatorA);
	});
var _elm_community$random_extra$Random_Extra$andThen5 = F6(
	function (constructor, generatorA, generatorB, generatorC, generatorD, generatorE) {
		return A2(
			_elm_lang$core$Random$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Random$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Random$andThen,
							function (c) {
								return A2(
									_elm_lang$core$Random$andThen,
									function (d) {
										return A2(
											_elm_lang$core$Random$andThen,
											function (e) {
												return A5(constructor, a, b, c, d, e);
											},
											generatorE);
									},
									generatorD);
							},
							generatorC);
					},
					generatorB);
			},
			generatorA);
	});
var _elm_community$random_extra$Random_Extra$andThen4 = F5(
	function (constructor, generatorA, generatorB, generatorC, generatorD) {
		return A2(
			_elm_lang$core$Random$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Random$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Random$andThen,
							function (c) {
								return A2(
									_elm_lang$core$Random$andThen,
									function (d) {
										return A4(constructor, a, b, c, d);
									},
									generatorD);
							},
							generatorC);
					},
					generatorB);
			},
			generatorA);
	});
var _elm_community$random_extra$Random_Extra$andThen3 = F4(
	function (constructor, generatorA, generatorB, generatorC) {
		return A2(
			_elm_lang$core$Random$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Random$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Random$andThen,
							function (c) {
								return A3(constructor, a, b, c);
							},
							generatorC);
					},
					generatorB);
			},
			generatorA);
	});
var _elm_community$random_extra$Random_Extra$andThen2 = F3(
	function (constructor, generatorA, generatorB) {
		return A2(
			_elm_lang$core$Random$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Random$andThen,
					function (b) {
						return A2(constructor, a, b);
					},
					generatorB);
			},
			generatorA);
	});
var _elm_community$random_extra$Random_Extra$rangeLengthList = F3(
	function (minLength, maxLength, generator) {
		return A2(
			_elm_lang$core$Random$andThen,
			function (len) {
				return A2(_elm_lang$core$Random$list, len, generator);
			},
			A2(_elm_lang$core$Random$int, minLength, maxLength));
	});
var _elm_community$random_extra$Random_Extra$result = F3(
	function (genBool, genErr, genVal) {
		return A2(
			_elm_lang$core$Random$andThen,
			function (b) {
				return b ? A2(_elm_lang$core$Random$map, _elm_lang$core$Result$Ok, genVal) : A2(_elm_lang$core$Random$map, _elm_lang$core$Result$Err, genErr);
			},
			genBool);
	});
var _elm_community$random_extra$Random_Extra$sample = function () {
	var find = F2(
		function (k, ys) {
			find:
			while (true) {
				var _p0 = ys;
				if (_p0.ctor === '[]') {
					return _elm_lang$core$Maybe$Nothing;
				} else {
					if (_elm_lang$core$Native_Utils.eq(k, 0)) {
						return _elm_lang$core$Maybe$Just(_p0._0);
					} else {
						var _v1 = k - 1,
							_v2 = _p0._1;
						k = _v1;
						ys = _v2;
						continue find;
					}
				}
			}
		});
	return function (xs) {
		return A2(
			_elm_lang$core$Random$map,
			function (i) {
				return A2(find, i, xs);
			},
			A2(
				_elm_lang$core$Random$int,
				0,
				_elm_lang$core$List$length(xs) - 1));
	};
}();
var _elm_community$random_extra$Random_Extra$frequency = function (pairs) {
	var pick = F2(
		function (choices, n) {
			pick:
			while (true) {
				var _p1 = choices;
				if ((_p1.ctor === '::') && (_p1._0.ctor === '_Tuple2')) {
					var _p2 = _p1._0._0;
					if (_elm_lang$core$Native_Utils.cmp(n, _p2) < 1) {
						return _p1._0._1;
					} else {
						var _v4 = _p1._1,
							_v5 = n - _p2;
						choices = _v4;
						n = _v5;
						continue pick;
					}
				} else {
					return _elm_lang$core$Native_Utils.crashCase(
						'Random.Extra',
						{
							start: {line: 154, column: 13},
							end: {line: 162, column: 79}
						},
						_p1)('Empty list passed to Random.Extra.frequency!');
				}
			}
		});
	var total = _elm_lang$core$List$sum(
		A2(
			_elm_lang$core$List$map,
			function (_p4) {
				return _elm_lang$core$Basics$abs(
					_elm_lang$core$Tuple$first(_p4));
			},
			pairs));
	return A2(
		_elm_lang$core$Random$andThen,
		pick(pairs),
		A2(_elm_lang$core$Random$float, 0, total));
};
var _elm_community$random_extra$Random_Extra$choices = function (gens) {
	return _elm_community$random_extra$Random_Extra$frequency(
		A2(
			_elm_lang$core$List$map,
			function (g) {
				return {ctor: '_Tuple2', _0: 1, _1: g};
			},
			gens));
};
var _elm_community$random_extra$Random_Extra$choice = F2(
	function (x, y) {
		return A2(
			_elm_lang$core$Random$map,
			function (b) {
				return b ? x : y;
			},
			_elm_lang$core$Random$bool);
	});
var _elm_community$random_extra$Random_Extra$oneIn = function (n) {
	return A2(
		_elm_lang$core$Random$map,
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.eq(x, y);
			})(1),
		A2(_elm_lang$core$Random$int, 1, n));
};
var _elm_community$random_extra$Random_Extra$andMap = F2(
	function (generator, funcGenerator) {
		return A3(
			_elm_lang$core$Random$map2,
			F2(
				function (x, y) {
					return x(y);
				}),
			funcGenerator,
			generator);
	});
var _elm_community$random_extra$Random_Extra$map6 = F7(
	function (f, generatorA, generatorB, generatorC, generatorD, generatorE, generatorF) {
		return A2(
			_elm_community$random_extra$Random_Extra$andMap,
			generatorF,
			A6(_elm_lang$core$Random$map5, f, generatorA, generatorB, generatorC, generatorD, generatorE));
	});
var _elm_community$random_extra$Random_Extra$constant = function (value) {
	return A2(
		_elm_lang$core$Random$map,
		function (_p5) {
			return value;
		},
		_elm_lang$core$Random$bool);
};
var _elm_community$random_extra$Random_Extra$filter = F2(
	function (predicate, generator) {
		return A2(
			_elm_lang$core$Random$andThen,
			function (a) {
				return predicate(a) ? _elm_community$random_extra$Random_Extra$constant(a) : A2(_elm_community$random_extra$Random_Extra$filter, predicate, generator);
			},
			generator);
	});
var _elm_community$random_extra$Random_Extra$combine = function (generators) {
	var _p6 = generators;
	if (_p6.ctor === '[]') {
		return _elm_community$random_extra$Random_Extra$constant(
			{ctor: '[]'});
	} else {
		return A3(
			_elm_lang$core$Random$map2,
			F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				}),
			_p6._0,
			_elm_community$random_extra$Random_Extra$combine(_p6._1));
	}
};
var _elm_community$random_extra$Random_Extra$maybe = F2(
	function (genBool, genA) {
		return A2(
			_elm_lang$core$Random$andThen,
			function (b) {
				return b ? A2(_elm_lang$core$Random$map, _elm_lang$core$Maybe$Just, genA) : _elm_community$random_extra$Random_Extra$constant(_elm_lang$core$Maybe$Nothing);
			},
			genBool);
	});

var _elm_community$random_extra$Random_Array$choose = function (arr) {
	if (_elm_lang$core$Array$isEmpty(arr)) {
		return _elm_community$random_extra$Random_Extra$constant(
			{ctor: '_Tuple2', _0: _elm_lang$core$Maybe$Nothing, _1: arr});
	} else {
		var front = function (i) {
			return A3(_elm_lang$core$Array$slice, 0, i, arr);
		};
		var lastIndex = _elm_lang$core$Array$length(arr) - 1;
		var back = function (i) {
			return _elm_lang$core$Native_Utils.eq(i, lastIndex) ? _elm_lang$core$Array$empty : A3(_elm_lang$core$Array$slice, i + 1, lastIndex + 1, arr);
		};
		var gen = A2(_elm_lang$core$Random$int, 0, lastIndex);
		return A2(
			_elm_lang$core$Random$map,
			function (index) {
				return {
					ctor: '_Tuple2',
					_0: A2(_elm_lang$core$Array$get, index, arr),
					_1: A2(
						_elm_lang$core$Array$append,
						front(index),
						back(index))
				};
			},
			gen);
	}
};
var _elm_community$random_extra$Random_Array$shuffle = function (arr) {
	if (_elm_lang$core$Array$isEmpty(arr)) {
		return _elm_community$random_extra$Random_Extra$constant(arr);
	} else {
		var helper = function (_p0) {
			var _p1 = _p0;
			var _p6 = _p1._0;
			return A2(
				_elm_lang$core$Random$andThen,
				function (_p2) {
					var _p3 = _p2;
					var _p5 = _p3._1;
					var _p4 = _p3._0;
					if (_p4.ctor === 'Nothing') {
						return _elm_community$random_extra$Random_Extra$constant(
							{ctor: '_Tuple2', _0: _p6, _1: _p5});
					} else {
						return helper(
							{
								ctor: '_Tuple2',
								_0: {ctor: '::', _0: _p4._0, _1: _p6},
								_1: _p5
							});
					}
				},
				_elm_community$random_extra$Random_Array$choose(_p1._1));
		};
		return A2(
			_elm_lang$core$Random$map,
			function (_p7) {
				return _elm_lang$core$Array$fromList(
					_elm_lang$core$Tuple$first(_p7));
			},
			helper(
				{
					ctor: '_Tuple2',
					_0: {ctor: '[]'},
					_1: arr
				}));
	}
};
var _elm_community$random_extra$Random_Array$sample = function (arr) {
	var gen = A2(
		_elm_lang$core$Random$int,
		0,
		_elm_lang$core$Array$length(arr) - 1);
	return A2(
		_elm_lang$core$Random$map,
		function (index) {
			return A2(_elm_lang$core$Array$get, index, arr);
		},
		gen);
};
var _elm_community$random_extra$Random_Array$array = F2(
	function (arrayLength, generator) {
		return A2(
			_elm_lang$core$Random$map,
			_elm_lang$core$Array$fromList,
			A2(_elm_lang$core$Random$list, arrayLength, generator));
	});
var _elm_community$random_extra$Random_Array$rangeLengthArray = F3(
	function (minLength, maxLength, generator) {
		return A2(
			_elm_lang$core$Random$andThen,
			function (len) {
				return A2(_elm_community$random_extra$Random_Array$array, len, generator);
			},
			A2(_elm_lang$core$Random$int, minLength, maxLength));
	});

//import Maybe, Native.List //

var _elm_lang$core$Native_Regex = function() {

function escape(str)
{
	return str.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
}
function caseInsensitive(re)
{
	return new RegExp(re.source, 'gi');
}
function regex(raw)
{
	return new RegExp(raw, 'g');
}

function contains(re, string)
{
	return string.match(re) !== null;
}

function find(n, re, str)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	var out = [];
	var number = 0;
	var string = str;
	var lastIndex = re.lastIndex;
	var prevLastIndex = -1;
	var result;
	while (number++ < n && (result = re.exec(string)))
	{
		if (prevLastIndex === re.lastIndex) break;
		var i = result.length - 1;
		var subs = new Array(i);
		while (i > 0)
		{
			var submatch = result[i];
			subs[--i] = submatch === undefined
				? _elm_lang$core$Maybe$Nothing
				: _elm_lang$core$Maybe$Just(submatch);
		}
		out.push({
			match: result[0],
			submatches: _elm_lang$core$Native_List.fromArray(subs),
			index: result.index,
			number: number
		});
		prevLastIndex = re.lastIndex;
	}
	re.lastIndex = lastIndex;
	return _elm_lang$core$Native_List.fromArray(out);
}

function replace(n, re, replacer, string)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	var count = 0;
	function jsReplacer(match)
	{
		if (count++ >= n)
		{
			return match;
		}
		var i = arguments.length - 3;
		var submatches = new Array(i);
		while (i > 0)
		{
			var submatch = arguments[i];
			submatches[--i] = submatch === undefined
				? _elm_lang$core$Maybe$Nothing
				: _elm_lang$core$Maybe$Just(submatch);
		}
		return replacer({
			match: match,
			submatches: _elm_lang$core$Native_List.fromArray(submatches),
			index: arguments[arguments.length - 2],
			number: count
		});
	}
	return string.replace(re, jsReplacer);
}

function split(n, re, str)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	if (n === Infinity)
	{
		return _elm_lang$core$Native_List.fromArray(str.split(re));
	}
	var string = str;
	var result;
	var out = [];
	var start = re.lastIndex;
	var restoreLastIndex = re.lastIndex;
	while (n--)
	{
		if (!(result = re.exec(string))) break;
		out.push(string.slice(start, result.index));
		start = re.lastIndex;
	}
	out.push(string.slice(start));
	re.lastIndex = restoreLastIndex;
	return _elm_lang$core$Native_List.fromArray(out);
}

return {
	regex: regex,
	caseInsensitive: caseInsensitive,
	escape: escape,

	contains: F2(contains),
	find: F3(find),
	replace: F4(replace),
	split: F3(split)
};

}();

var _elm_lang$core$Regex$split = _elm_lang$core$Native_Regex.split;
var _elm_lang$core$Regex$replace = _elm_lang$core$Native_Regex.replace;
var _elm_lang$core$Regex$find = _elm_lang$core$Native_Regex.find;
var _elm_lang$core$Regex$contains = _elm_lang$core$Native_Regex.contains;
var _elm_lang$core$Regex$caseInsensitive = _elm_lang$core$Native_Regex.caseInsensitive;
var _elm_lang$core$Regex$regex = _elm_lang$core$Native_Regex.regex;
var _elm_lang$core$Regex$escape = _elm_lang$core$Native_Regex.escape;
var _elm_lang$core$Regex$Match = F4(
	function (a, b, c, d) {
		return {match: a, submatches: b, index: c, number: d};
	});
var _elm_lang$core$Regex$Regex = {ctor: 'Regex'};
var _elm_lang$core$Regex$AtMost = function (a) {
	return {ctor: 'AtMost', _0: a};
};
var _elm_lang$core$Regex$All = {ctor: 'All'};

var _elm_lang$core$Native_Bitwise = function() {

return {
	and: F2(function and(a, b) { return a & b; }),
	or: F2(function or(a, b) { return a | b; }),
	xor: F2(function xor(a, b) { return a ^ b; }),
	complement: function complement(a) { return ~a; },
	shiftLeftBy: F2(function(offset, a) { return a << offset; }),
	shiftRightBy: F2(function(offset, a) { return a >> offset; }),
	shiftRightZfBy: F2(function(offset, a) { return a >>> offset; })
};

}();

var _elm_lang$core$Bitwise$shiftRightZfBy = _elm_lang$core$Native_Bitwise.shiftRightZfBy;
var _elm_lang$core$Bitwise$shiftRightBy = _elm_lang$core$Native_Bitwise.shiftRightBy;
var _elm_lang$core$Bitwise$shiftLeftBy = _elm_lang$core$Native_Bitwise.shiftLeftBy;
var _elm_lang$core$Bitwise$complement = _elm_lang$core$Native_Bitwise.complement;
var _elm_lang$core$Bitwise$xor = _elm_lang$core$Native_Bitwise.xor;
var _elm_lang$core$Bitwise$or = _elm_lang$core$Native_Bitwise.or;
var _elm_lang$core$Bitwise$and = _elm_lang$core$Native_Bitwise.and;

var _elm_community$string_extra$String_Extra$accentRegex = function () {
	var matches = {
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: '[à-æ]', _1: 'a'},
		_1: {
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: '[À-Æ]', _1: 'A'},
			_1: {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 'ç', _1: 'c'},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'Ç', _1: 'C'},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: '[è-ë]', _1: 'e'},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: '[È-Ë]', _1: 'E'},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: '[ì-ï]', _1: 'i'},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: '[Ì-Ï]', _1: 'I'},
									_1: {
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'ñ', _1: 'n'},
										_1: {
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 'Ñ', _1: 'N'},
											_1: {
												ctor: '::',
												_0: {ctor: '_Tuple2', _0: '[ò-ö]', _1: 'o'},
												_1: {
													ctor: '::',
													_0: {ctor: '_Tuple2', _0: '[Ò-Ö]', _1: 'O'},
													_1: {
														ctor: '::',
														_0: {ctor: '_Tuple2', _0: '[ù-ü]', _1: 'u'},
														_1: {
															ctor: '::',
															_0: {ctor: '_Tuple2', _0: '[Ù-Ü]', _1: 'U'},
															_1: {
																ctor: '::',
																_0: {ctor: '_Tuple2', _0: 'ý', _1: 'y'},
																_1: {
																	ctor: '::',
																	_0: {ctor: '_Tuple2', _0: 'ÿ', _1: 'y'},
																	_1: {
																		ctor: '::',
																		_0: {ctor: '_Tuple2', _0: 'Ý', _1: 'Y'},
																		_1: {ctor: '[]'}
																	}
																}
															}
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	};
	return A2(
		_elm_lang$core$List$map,
		function (_p0) {
			var _p1 = _p0;
			return {
				ctor: '_Tuple2',
				_0: _elm_lang$core$Regex$regex(_p1._0),
				_1: _p1._1
			};
		},
		matches);
}();
var _elm_community$string_extra$String_Extra$removeAccents = function (string) {
	if (_elm_lang$core$String$isEmpty(string)) {
		return string;
	} else {
		var do_regex_to_remove_acents = function (_p2) {
			var _p3 = _p2;
			return A3(
				_elm_lang$core$Regex$replace,
				_elm_lang$core$Regex$All,
				_p3._0,
				function (_p4) {
					return _p3._1;
				});
		};
		return A3(_elm_lang$core$List$foldl, do_regex_to_remove_acents, string, _elm_community$string_extra$String_Extra$accentRegex);
	}
};
var _elm_community$string_extra$String_Extra$nonEmpty = function (string) {
	return _elm_lang$core$String$isEmpty(string) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(string);
};
var _elm_community$string_extra$String_Extra$replacementCodePoint = 65533;
var _elm_community$string_extra$String_Extra$toCodePoints = function (string) {
	var allCodeUnits = A2(
		_elm_lang$core$List$map,
		_elm_lang$core$Char$toCode,
		_elm_lang$core$String$toList(string));
	var combineAndReverse = F2(
		function (codeUnits, accumulated) {
			combineAndReverse:
			while (true) {
				var _p5 = codeUnits;
				if (_p5.ctor === '[]') {
					return accumulated;
				} else {
					var _p9 = _p5._0;
					var _p8 = _p5._1;
					if ((_elm_lang$core$Native_Utils.cmp(_p9, 0) > -1) && (_elm_lang$core$Native_Utils.cmp(_p9, 55295) < 1)) {
						var _v3 = _p8,
							_v4 = {ctor: '::', _0: _p9, _1: accumulated};
						codeUnits = _v3;
						accumulated = _v4;
						continue combineAndReverse;
					} else {
						if ((_elm_lang$core$Native_Utils.cmp(_p9, 55296) > -1) && (_elm_lang$core$Native_Utils.cmp(_p9, 56319) < 1)) {
							var _p6 = _p8;
							if (_p6.ctor === '[]') {
								return {ctor: '::', _0: _elm_community$string_extra$String_Extra$replacementCodePoint, _1: accumulated};
							} else {
								var _p7 = _p6._0;
								if ((_elm_lang$core$Native_Utils.cmp(_p7, 56320) > -1) && (_elm_lang$core$Native_Utils.cmp(_p7, 57343) < 1)) {
									var codePoint = (65536 + ((_p9 - 55296) * 1024)) + (_p7 - 56320);
									var _v6 = _p6._1,
										_v7 = {ctor: '::', _0: codePoint, _1: accumulated};
									codeUnits = _v6;
									accumulated = _v7;
									continue combineAndReverse;
								} else {
									var _v8 = _p8,
										_v9 = {ctor: '::', _0: _elm_community$string_extra$String_Extra$replacementCodePoint, _1: accumulated};
									codeUnits = _v8;
									accumulated = _v9;
									continue combineAndReverse;
								}
							}
						} else {
							if ((_elm_lang$core$Native_Utils.cmp(_p9, 57344) > -1) && (_elm_lang$core$Native_Utils.cmp(_p9, 65535) < 1)) {
								var _v10 = _p8,
									_v11 = {ctor: '::', _0: _p9, _1: accumulated};
								codeUnits = _v10;
								accumulated = _v11;
								continue combineAndReverse;
							} else {
								var _v12 = _p8,
									_v13 = {ctor: '::', _0: _elm_community$string_extra$String_Extra$replacementCodePoint, _1: accumulated};
								codeUnits = _v12;
								accumulated = _v13;
								continue combineAndReverse;
							}
						}
					}
				}
			}
		});
	return _elm_lang$core$List$reverse(
		A2(
			combineAndReverse,
			allCodeUnits,
			{ctor: '[]'}));
};
var _elm_community$string_extra$String_Extra$fromCodePoints = function (allCodePoints) {
	var splitAndReverse = F2(
		function (codePoints, accumulated) {
			splitAndReverse:
			while (true) {
				var _p10 = codePoints;
				if (_p10.ctor === '[]') {
					return accumulated;
				} else {
					var _p12 = _p10._1;
					var _p11 = _p10._0;
					if ((_elm_lang$core$Native_Utils.cmp(_p11, 0) > -1) && (_elm_lang$core$Native_Utils.cmp(_p11, 55295) < 1)) {
						var _v15 = _p12,
							_v16 = {ctor: '::', _0: _p11, _1: accumulated};
						codePoints = _v15;
						accumulated = _v16;
						continue splitAndReverse;
					} else {
						if ((_elm_lang$core$Native_Utils.cmp(_p11, 65536) > -1) && (_elm_lang$core$Native_Utils.cmp(_p11, 1114111) < 1)) {
							var subtracted = _p11 - 65536;
							var leading = (subtracted >> 10) + 55296;
							var trailing = (subtracted & 1023) + 56320;
							var _v17 = _p12,
								_v18 = {
								ctor: '::',
								_0: trailing,
								_1: {ctor: '::', _0: leading, _1: accumulated}
							};
							codePoints = _v17;
							accumulated = _v18;
							continue splitAndReverse;
						} else {
							if ((_elm_lang$core$Native_Utils.cmp(_p11, 57344) > -1) && (_elm_lang$core$Native_Utils.cmp(_p11, 65535) < 1)) {
								var _v19 = _p12,
									_v20 = {ctor: '::', _0: _p11, _1: accumulated};
								codePoints = _v19;
								accumulated = _v20;
								continue splitAndReverse;
							} else {
								var _v21 = _p12,
									_v22 = {ctor: '::', _0: _elm_community$string_extra$String_Extra$replacementCodePoint, _1: accumulated};
								codePoints = _v21;
								accumulated = _v22;
								continue splitAndReverse;
							}
						}
					}
				}
			}
		});
	var allCodeUnits = _elm_lang$core$List$reverse(
		A2(
			splitAndReverse,
			allCodePoints,
			{ctor: '[]'}));
	return _elm_lang$core$String$fromList(
		A2(_elm_lang$core$List$map, _elm_lang$core$Char$fromCode, allCodeUnits));
};
var _elm_community$string_extra$String_Extra$fromFloat = _elm_lang$core$Basics$toString;
var _elm_community$string_extra$String_Extra$fromInt = _elm_lang$core$Basics$toString;
var _elm_community$string_extra$String_Extra$leftOfBack = F2(
	function (pattern, string) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			'',
			A2(
				_elm_lang$core$Maybe$map,
				A2(_elm_lang$core$Basics$flip, _elm_lang$core$String$left, string),
				_elm_lang$core$List$head(
					_elm_lang$core$List$reverse(
						A2(_elm_lang$core$String$indexes, pattern, string)))));
	});
var _elm_community$string_extra$String_Extra$rightOfBack = F2(
	function (pattern, string) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			'',
			A2(
				_elm_lang$core$Maybe$map,
				function (_p13) {
					return A3(
						_elm_lang$core$Basics$flip,
						_elm_lang$core$String$dropLeft,
						string,
						A2(
							F2(
								function (x, y) {
									return x + y;
								}),
							_elm_lang$core$String$length(pattern),
							_p13));
				},
				_elm_lang$core$List$head(
					_elm_lang$core$List$reverse(
						A2(_elm_lang$core$String$indexes, pattern, string)))));
	});
var _elm_community$string_extra$String_Extra$firstResultHelp = F2(
	function ($default, list) {
		firstResultHelp:
		while (true) {
			var _p14 = list;
			if (_p14.ctor === '[]') {
				return $default;
			} else {
				if (_p14._0.ctor === 'Just') {
					return _p14._0._0;
				} else {
					var _v24 = $default,
						_v25 = _p14._1;
					$default = _v24;
					list = _v25;
					continue firstResultHelp;
				}
			}
		}
	});
var _elm_community$string_extra$String_Extra$firstResult = function (list) {
	return A2(_elm_community$string_extra$String_Extra$firstResultHelp, '', list);
};
var _elm_community$string_extra$String_Extra$leftOf = F2(
	function (pattern, string) {
		return A2(
			_elm_lang$core$String$join,
			'',
			A2(
				_elm_lang$core$List$map,
				function (_p15) {
					return _elm_community$string_extra$String_Extra$firstResult(
						function (_) {
							return _.submatches;
						}(_p15));
				},
				A3(
					_elm_lang$core$Regex$find,
					_elm_lang$core$Regex$AtMost(1),
					_elm_lang$core$Regex$regex(
						A2(
							_elm_lang$core$Basics_ops['++'],
							'^(.*?)',
							_elm_lang$core$Regex$escape(pattern))),
					string)));
	});
var _elm_community$string_extra$String_Extra$rightOf = F2(
	function (pattern, string) {
		return A2(
			_elm_lang$core$String$join,
			'',
			A2(
				_elm_lang$core$List$map,
				function (_p16) {
					return _elm_community$string_extra$String_Extra$firstResult(
						function (_) {
							return _.submatches;
						}(_p16));
				},
				A3(
					_elm_lang$core$Regex$find,
					_elm_lang$core$Regex$AtMost(1),
					_elm_lang$core$Regex$regex(
						A2(
							_elm_lang$core$Basics_ops['++'],
							_elm_lang$core$Regex$escape(pattern),
							'(.*)$')),
					string)));
	});
var _elm_community$string_extra$String_Extra$pluralize = F3(
	function (singular, plural, count) {
		return _elm_lang$core$Native_Utils.eq(count, 1) ? A2(_elm_lang$core$Basics_ops['++'], '1 ', singular) : A2(
			_elm_lang$core$Basics_ops['++'],
			_elm_lang$core$Basics$toString(count),
			A2(_elm_lang$core$Basics_ops['++'], ' ', plural));
	});
var _elm_community$string_extra$String_Extra$stripTags = function (string) {
	return A4(
		_elm_lang$core$Regex$replace,
		_elm_lang$core$Regex$All,
		_elm_lang$core$Regex$regex('<\\/?[^>]+>'),
		_elm_lang$core$Basics$always(''),
		string);
};
var _elm_community$string_extra$String_Extra$toSentenceHelper = F3(
	function (lastPart, sentence, list) {
		toSentenceHelper:
		while (true) {
			var _p17 = list;
			if (_p17.ctor === '[]') {
				return sentence;
			} else {
				if (_p17._1.ctor === '[]') {
					return A2(
						_elm_lang$core$Basics_ops['++'],
						sentence,
						A2(_elm_lang$core$Basics_ops['++'], lastPart, _p17._0));
				} else {
					var _v27 = lastPart,
						_v28 = A2(
						_elm_lang$core$Basics_ops['++'],
						sentence,
						A2(_elm_lang$core$Basics_ops['++'], ', ', _p17._0)),
						_v29 = _p17._1;
					lastPart = _v27;
					sentence = _v28;
					list = _v29;
					continue toSentenceHelper;
				}
			}
		}
	});
var _elm_community$string_extra$String_Extra$toSentenceBaseCase = function (list) {
	var _p18 = list;
	_v30_2:
	do {
		if (_p18.ctor === '::') {
			if (_p18._1.ctor === '[]') {
				return _p18._0;
			} else {
				if (_p18._1._1.ctor === '[]') {
					return A2(
						_elm_lang$core$Basics_ops['++'],
						_p18._0,
						A2(_elm_lang$core$Basics_ops['++'], ' and ', _p18._1._0));
				} else {
					break _v30_2;
				}
			}
		} else {
			break _v30_2;
		}
	} while(false);
	return '';
};
var _elm_community$string_extra$String_Extra$toSentenceOxford = function (list) {
	var _p19 = list;
	if (((_p19.ctor === '::') && (_p19._1.ctor === '::')) && (_p19._1._1.ctor === '::')) {
		return A3(
			_elm_community$string_extra$String_Extra$toSentenceHelper,
			', and ',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_p19._0,
				A2(_elm_lang$core$Basics_ops['++'], ', ', _p19._1._0)),
			{ctor: '::', _0: _p19._1._1._0, _1: _p19._1._1._1});
	} else {
		return _elm_community$string_extra$String_Extra$toSentenceBaseCase(list);
	}
};
var _elm_community$string_extra$String_Extra$toSentence = function (list) {
	var _p20 = list;
	if (((_p20.ctor === '::') && (_p20._1.ctor === '::')) && (_p20._1._1.ctor === '::')) {
		return A3(
			_elm_community$string_extra$String_Extra$toSentenceHelper,
			' and ',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_p20._0,
				A2(_elm_lang$core$Basics_ops['++'], ', ', _p20._1._0)),
			{ctor: '::', _0: _p20._1._1._0, _1: _p20._1._1._1});
	} else {
		return _elm_community$string_extra$String_Extra$toSentenceBaseCase(list);
	}
};
var _elm_community$string_extra$String_Extra$ellipsisWith = F3(
	function (howLong, append, string) {
		return (_elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$String$length(string),
			howLong) < 1) ? string : A2(
			_elm_lang$core$Basics_ops['++'],
			A2(
				_elm_lang$core$String$left,
				howLong - _elm_lang$core$String$length(append),
				string),
			append);
	});
var _elm_community$string_extra$String_Extra$ellipsis = F2(
	function (howLong, string) {
		return A3(_elm_community$string_extra$String_Extra$ellipsisWith, howLong, '...', string);
	});
var _elm_community$string_extra$String_Extra$countOccurrences = F2(
	function (needle, haystack) {
		return (_elm_lang$core$Native_Utils.eq(
			_elm_lang$core$String$length(needle),
			0) || _elm_lang$core$Native_Utils.eq(
			_elm_lang$core$String$length(haystack),
			0)) ? 0 : _elm_lang$core$List$length(
			A2(_elm_lang$core$String$indexes, needle, haystack));
	});
var _elm_community$string_extra$String_Extra$unindent = function (multilineSting) {
	var isNotWhitespace = function ($char) {
		return (!_elm_lang$core$Native_Utils.eq(
			$char,
			_elm_lang$core$Native_Utils.chr(' '))) && (!_elm_lang$core$Native_Utils.eq(
			$char,
			_elm_lang$core$Native_Utils.chr('\t')));
	};
	var countLeadingWhitespace = F2(
		function (count, line) {
			countLeadingWhitespace:
			while (true) {
				var _p21 = _elm_lang$core$String$uncons(line);
				if (_p21.ctor === 'Nothing') {
					return count;
				} else {
					var _p23 = _p21._0._1;
					var _p22 = _p21._0._0;
					switch (_p22.valueOf()) {
						case ' ':
							var _v35 = count + 1,
								_v36 = _p23;
							count = _v35;
							line = _v36;
							continue countLeadingWhitespace;
						case '\t':
							var _v37 = count + 1,
								_v38 = _p23;
							count = _v37;
							line = _v38;
							continue countLeadingWhitespace;
						default:
							return count;
					}
				}
			}
		});
	var lines = _elm_lang$core$String$lines(multilineSting);
	var minLead = A2(
		_elm_lang$core$Maybe$withDefault,
		0,
		_elm_lang$core$List$minimum(
			A2(
				_elm_lang$core$List$map,
				countLeadingWhitespace(0),
				A2(
					_elm_lang$core$List$filter,
					_elm_lang$core$String$any(isNotWhitespace),
					lines))));
	return A2(
		_elm_lang$core$String$join,
		'\n',
		A2(
			_elm_lang$core$List$map,
			_elm_lang$core$String$dropLeft(minLead),
			lines));
};
var _elm_community$string_extra$String_Extra$dasherize = function (string) {
	return _elm_lang$core$String$toLower(
		A4(
			_elm_lang$core$Regex$replace,
			_elm_lang$core$Regex$All,
			_elm_lang$core$Regex$regex('[_-\\s]+'),
			_elm_lang$core$Basics$always('-'),
			A4(
				_elm_lang$core$Regex$replace,
				_elm_lang$core$Regex$All,
				_elm_lang$core$Regex$regex('([A-Z])'),
				function (_p24) {
					return A2(
						_elm_lang$core$String$append,
						'-',
						function (_) {
							return _.match;
						}(_p24));
				},
				_elm_lang$core$String$trim(string))));
};
var _elm_community$string_extra$String_Extra$underscored = function (string) {
	return _elm_lang$core$String$toLower(
		A4(
			_elm_lang$core$Regex$replace,
			_elm_lang$core$Regex$All,
			_elm_lang$core$Regex$regex('[_-\\s]+'),
			_elm_lang$core$Basics$always('_'),
			A4(
				_elm_lang$core$Regex$replace,
				_elm_lang$core$Regex$All,
				_elm_lang$core$Regex$regex('([a-z\\d])([A-Z]+)'),
				function (_p25) {
					return A2(
						_elm_lang$core$String$join,
						'_',
						A2(
							_elm_lang$core$List$filterMap,
							_elm_lang$core$Basics$identity,
							function (_) {
								return _.submatches;
							}(_p25)));
				},
				_elm_lang$core$String$trim(string))));
};
var _elm_community$string_extra$String_Extra$unsurround = F2(
	function (wrap, string) {
		if (A2(_elm_lang$core$String$startsWith, wrap, string) && A2(_elm_lang$core$String$endsWith, wrap, string)) {
			var length = _elm_lang$core$String$length(wrap);
			return A2(
				_elm_lang$core$String$dropRight,
				length,
				A2(_elm_lang$core$String$dropLeft, length, string));
		} else {
			return string;
		}
	});
var _elm_community$string_extra$String_Extra$unquote = function (string) {
	return A2(_elm_community$string_extra$String_Extra$unsurround, '\"', string);
};
var _elm_community$string_extra$String_Extra$surround = F2(
	function (wrap, string) {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			wrap,
			A2(_elm_lang$core$Basics_ops['++'], string, wrap));
	});
var _elm_community$string_extra$String_Extra$quote = function (string) {
	return A2(_elm_community$string_extra$String_Extra$surround, '\"', string);
};
var _elm_community$string_extra$String_Extra$camelize = function (string) {
	return A4(
		_elm_lang$core$Regex$replace,
		_elm_lang$core$Regex$All,
		_elm_lang$core$Regex$regex('[-_\\s]+(.)?'),
		function (_p26) {
			var _p27 = _p26;
			var _p28 = _p27.submatches;
			if ((_p28.ctor === '::') && (_p28._0.ctor === 'Just')) {
				return _elm_lang$core$String$toUpper(_p28._0._0);
			} else {
				return '';
			}
		},
		_elm_lang$core$String$trim(string));
};
var _elm_community$string_extra$String_Extra$isBlank = function (string) {
	return A2(
		_elm_lang$core$Regex$contains,
		_elm_lang$core$Regex$regex('^\\s*$'),
		string);
};
var _elm_community$string_extra$String_Extra$clean = function (string) {
	return _elm_lang$core$String$trim(
		A4(
			_elm_lang$core$Regex$replace,
			_elm_lang$core$Regex$All,
			_elm_lang$core$Regex$regex('\\s\\s+'),
			_elm_lang$core$Basics$always(' '),
			string));
};
var _elm_community$string_extra$String_Extra$softBreakRegexp = function (width) {
	return _elm_lang$core$Regex$regex(
		A2(
			_elm_lang$core$Basics_ops['++'],
			'.{1,',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$Basics$toString(width),
				'}(\\s+|$)|\\S+?(\\s+|$)')));
};
var _elm_community$string_extra$String_Extra$softEllipsis = F2(
	function (howLong, string) {
		return (_elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$String$length(string),
			howLong) < 1) ? string : A3(
			_elm_lang$core$Basics$flip,
			_elm_lang$core$String$append,
			'...',
			A4(
				_elm_lang$core$Regex$replace,
				_elm_lang$core$Regex$All,
				_elm_lang$core$Regex$regex('([\\.,;:\\s])+$'),
				_elm_lang$core$Basics$always(''),
				A2(
					_elm_lang$core$String$join,
					'',
					A2(
						_elm_lang$core$List$map,
						function (_) {
							return _.match;
						},
						A3(
							_elm_lang$core$Regex$find,
							_elm_lang$core$Regex$AtMost(1),
							_elm_community$string_extra$String_Extra$softBreakRegexp(howLong),
							string)))));
	});
var _elm_community$string_extra$String_Extra$softBreak = F2(
	function (width, string) {
		return (_elm_lang$core$Native_Utils.cmp(width, 0) < 1) ? {ctor: '[]'} : A2(
			_elm_lang$core$List$map,
			function (_) {
				return _.match;
			},
			A3(
				_elm_lang$core$Regex$find,
				_elm_lang$core$Regex$All,
				_elm_community$string_extra$String_Extra$softBreakRegexp(width),
				string));
	});
var _elm_community$string_extra$String_Extra$softWrapWith = F3(
	function (width, separator, string) {
		return A2(
			_elm_lang$core$String$join,
			separator,
			A2(_elm_community$string_extra$String_Extra$softBreak, width, string));
	});
var _elm_community$string_extra$String_Extra$softWrap = F2(
	function (width, string) {
		return A3(_elm_community$string_extra$String_Extra$softWrapWith, width, '\n', string);
	});
var _elm_community$string_extra$String_Extra$breaker = F3(
	function (width, string, acc) {
		breaker:
		while (true) {
			var _p29 = string;
			if (_p29 === '') {
				return _elm_lang$core$List$reverse(acc);
			} else {
				var _v42 = width,
					_v43 = A2(_elm_lang$core$String$dropLeft, width, string),
					_v44 = {
					ctor: '::',
					_0: A3(_elm_lang$core$String$slice, 0, width, string),
					_1: acc
				};
				width = _v42;
				string = _v43;
				acc = _v44;
				continue breaker;
			}
		}
	});
var _elm_community$string_extra$String_Extra$break = F2(
	function (width, string) {
		return (_elm_lang$core$Native_Utils.eq(width, 0) || _elm_lang$core$Native_Utils.eq(string, '')) ? {
			ctor: '::',
			_0: string,
			_1: {ctor: '[]'}
		} : A3(
			_elm_community$string_extra$String_Extra$breaker,
			width,
			string,
			{ctor: '[]'});
	});
var _elm_community$string_extra$String_Extra$wrapWith = F3(
	function (width, separator, string) {
		return A2(
			_elm_lang$core$String$join,
			separator,
			A2(_elm_community$string_extra$String_Extra$break, width, string));
	});
var _elm_community$string_extra$String_Extra$wrap = F2(
	function (width, string) {
		return A3(_elm_community$string_extra$String_Extra$wrapWith, width, '\n', string);
	});
var _elm_community$string_extra$String_Extra$replaceSlice = F4(
	function (substitution, start, end, string) {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			A3(_elm_lang$core$String$slice, 0, start, string),
			A2(
				_elm_lang$core$Basics_ops['++'],
				substitution,
				A3(
					_elm_lang$core$String$slice,
					end,
					_elm_lang$core$String$length(string),
					string)));
	});
var _elm_community$string_extra$String_Extra$insertAt = F3(
	function (insert, pos, string) {
		return A4(_elm_community$string_extra$String_Extra$replaceSlice, insert, pos, pos, string);
	});
var _elm_community$string_extra$String_Extra$replace = F3(
	function (search, substitution, string) {
		return A4(
			_elm_lang$core$Regex$replace,
			_elm_lang$core$Regex$All,
			_elm_lang$core$Regex$regex(
				_elm_lang$core$Regex$escape(search)),
			function (_p30) {
				return substitution;
			},
			string);
	});
var _elm_community$string_extra$String_Extra$changeCase = F2(
	function (mutator, word) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			'',
			A2(
				_elm_lang$core$Maybe$map,
				function (_p31) {
					var _p32 = _p31;
					return A2(
						_elm_lang$core$String$cons,
						mutator(_p32._0),
						_p32._1);
				},
				_elm_lang$core$String$uncons(word)));
	});
var _elm_community$string_extra$String_Extra$toSentenceCase = function (word) {
	return A2(_elm_community$string_extra$String_Extra$changeCase, _elm_lang$core$Char$toUpper, word);
};
var _elm_community$string_extra$String_Extra$toTitleCase = function (ws) {
	var uppercaseMatch = A3(
		_elm_lang$core$Regex$replace,
		_elm_lang$core$Regex$All,
		_elm_lang$core$Regex$regex('\\w+'),
		function (_p33) {
			return _elm_community$string_extra$String_Extra$toSentenceCase(
				function (_) {
					return _.match;
				}(_p33));
		});
	return A4(
		_elm_lang$core$Regex$replace,
		_elm_lang$core$Regex$All,
		_elm_lang$core$Regex$regex('^([a-z])|\\s+([a-z])'),
		function (_p34) {
			return uppercaseMatch(
				function (_) {
					return _.match;
				}(_p34));
		},
		ws);
};
var _elm_community$string_extra$String_Extra$classify = function (string) {
	return _elm_community$string_extra$String_Extra$toSentenceCase(
		A3(
			_elm_community$string_extra$String_Extra$replace,
			' ',
			'',
			_elm_community$string_extra$String_Extra$camelize(
				A4(
					_elm_lang$core$Regex$replace,
					_elm_lang$core$Regex$All,
					_elm_lang$core$Regex$regex('[\\W_]'),
					_elm_lang$core$Basics$always(' '),
					string))));
};
var _elm_community$string_extra$String_Extra$humanize = function (string) {
	return _elm_community$string_extra$String_Extra$toSentenceCase(
		_elm_lang$core$String$toLower(
			_elm_lang$core$String$trim(
				A4(
					_elm_lang$core$Regex$replace,
					_elm_lang$core$Regex$All,
					_elm_lang$core$Regex$regex('_id$|[-_\\s]+'),
					_elm_lang$core$Basics$always(' '),
					A4(
						_elm_lang$core$Regex$replace,
						_elm_lang$core$Regex$All,
						_elm_lang$core$Regex$regex('[A-Z]'),
						function (_p35) {
							return A2(
								_elm_lang$core$String$append,
								'-',
								function (_) {
									return _.match;
								}(_p35));
						},
						string)))));
};
var _elm_community$string_extra$String_Extra$decapitalize = function (word) {
	return A2(_elm_community$string_extra$String_Extra$changeCase, _elm_lang$core$Char$toLower, word);
};

//import Maybe, Native.Array, Native.List, Native.Utils, Result //

var _elm_lang$core$Native_Json = function() {


// CORE DECODERS

function succeed(msg)
{
	return {
		ctor: '<decoder>',
		tag: 'succeed',
		msg: msg
	};
}

function fail(msg)
{
	return {
		ctor: '<decoder>',
		tag: 'fail',
		msg: msg
	};
}

function decodePrimitive(tag)
{
	return {
		ctor: '<decoder>',
		tag: tag
	};
}

function decodeContainer(tag, decoder)
{
	return {
		ctor: '<decoder>',
		tag: tag,
		decoder: decoder
	};
}

function decodeNull(value)
{
	return {
		ctor: '<decoder>',
		tag: 'null',
		value: value
	};
}

function decodeField(field, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'field',
		field: field,
		decoder: decoder
	};
}

function decodeIndex(index, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'index',
		index: index,
		decoder: decoder
	};
}

function decodeKeyValuePairs(decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'key-value',
		decoder: decoder
	};
}

function mapMany(f, decoders)
{
	return {
		ctor: '<decoder>',
		tag: 'map-many',
		func: f,
		decoders: decoders
	};
}

function andThen(callback, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'andThen',
		decoder: decoder,
		callback: callback
	};
}

function oneOf(decoders)
{
	return {
		ctor: '<decoder>',
		tag: 'oneOf',
		decoders: decoders
	};
}


// DECODING OBJECTS

function map1(f, d1)
{
	return mapMany(f, [d1]);
}

function map2(f, d1, d2)
{
	return mapMany(f, [d1, d2]);
}

function map3(f, d1, d2, d3)
{
	return mapMany(f, [d1, d2, d3]);
}

function map4(f, d1, d2, d3, d4)
{
	return mapMany(f, [d1, d2, d3, d4]);
}

function map5(f, d1, d2, d3, d4, d5)
{
	return mapMany(f, [d1, d2, d3, d4, d5]);
}

function map6(f, d1, d2, d3, d4, d5, d6)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6]);
}

function map7(f, d1, d2, d3, d4, d5, d6, d7)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
}

function map8(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
}


// DECODE HELPERS

function ok(value)
{
	return { tag: 'ok', value: value };
}

function badPrimitive(type, value)
{
	return { tag: 'primitive', type: type, value: value };
}

function badIndex(index, nestedProblems)
{
	return { tag: 'index', index: index, rest: nestedProblems };
}

function badField(field, nestedProblems)
{
	return { tag: 'field', field: field, rest: nestedProblems };
}

function badIndex(index, nestedProblems)
{
	return { tag: 'index', index: index, rest: nestedProblems };
}

function badOneOf(problems)
{
	return { tag: 'oneOf', problems: problems };
}

function bad(msg)
{
	return { tag: 'fail', msg: msg };
}

function badToString(problem)
{
	var context = '_';
	while (problem)
	{
		switch (problem.tag)
		{
			case 'primitive':
				return 'Expecting ' + problem.type
					+ (context === '_' ? '' : ' at ' + context)
					+ ' but instead got: ' + jsToString(problem.value);

			case 'index':
				context += '[' + problem.index + ']';
				problem = problem.rest;
				break;

			case 'field':
				context += '.' + problem.field;
				problem = problem.rest;
				break;

			case 'oneOf':
				var problems = problem.problems;
				for (var i = 0; i < problems.length; i++)
				{
					problems[i] = badToString(problems[i]);
				}
				return 'I ran into the following problems'
					+ (context === '_' ? '' : ' at ' + context)
					+ ':\n\n' + problems.join('\n');

			case 'fail':
				return 'I ran into a `fail` decoder'
					+ (context === '_' ? '' : ' at ' + context)
					+ ': ' + problem.msg;
		}
	}
}

function jsToString(value)
{
	return value === undefined
		? 'undefined'
		: JSON.stringify(value);
}


// DECODE

function runOnString(decoder, string)
{
	var json;
	try
	{
		json = JSON.parse(string);
	}
	catch (e)
	{
		return _elm_lang$core$Result$Err('Given an invalid JSON: ' + e.message);
	}
	return run(decoder, json);
}

function run(decoder, value)
{
	var result = runHelp(decoder, value);
	return (result.tag === 'ok')
		? _elm_lang$core$Result$Ok(result.value)
		: _elm_lang$core$Result$Err(badToString(result));
}

function runHelp(decoder, value)
{
	switch (decoder.tag)
	{
		case 'bool':
			return (typeof value === 'boolean')
				? ok(value)
				: badPrimitive('a Bool', value);

		case 'int':
			if (typeof value !== 'number') {
				return badPrimitive('an Int', value);
			}

			if (-2147483647 < value && value < 2147483647 && (value | 0) === value) {
				return ok(value);
			}

			if (isFinite(value) && !(value % 1)) {
				return ok(value);
			}

			return badPrimitive('an Int', value);

		case 'float':
			return (typeof value === 'number')
				? ok(value)
				: badPrimitive('a Float', value);

		case 'string':
			return (typeof value === 'string')
				? ok(value)
				: (value instanceof String)
					? ok(value + '')
					: badPrimitive('a String', value);

		case 'null':
			return (value === null)
				? ok(decoder.value)
				: badPrimitive('null', value);

		case 'value':
			return ok(value);

		case 'list':
			if (!(value instanceof Array))
			{
				return badPrimitive('a List', value);
			}

			var list = _elm_lang$core$Native_List.Nil;
			for (var i = value.length; i--; )
			{
				var result = runHelp(decoder.decoder, value[i]);
				if (result.tag !== 'ok')
				{
					return badIndex(i, result)
				}
				list = _elm_lang$core$Native_List.Cons(result.value, list);
			}
			return ok(list);

		case 'array':
			if (!(value instanceof Array))
			{
				return badPrimitive('an Array', value);
			}

			var len = value.length;
			var array = new Array(len);
			for (var i = len; i--; )
			{
				var result = runHelp(decoder.decoder, value[i]);
				if (result.tag !== 'ok')
				{
					return badIndex(i, result);
				}
				array[i] = result.value;
			}
			return ok(_elm_lang$core$Native_Array.fromJSArray(array));

		case 'maybe':
			var result = runHelp(decoder.decoder, value);
			return (result.tag === 'ok')
				? ok(_elm_lang$core$Maybe$Just(result.value))
				: ok(_elm_lang$core$Maybe$Nothing);

		case 'field':
			var field = decoder.field;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return badPrimitive('an object with a field named `' + field + '`', value);
			}

			var result = runHelp(decoder.decoder, value[field]);
			return (result.tag === 'ok') ? result : badField(field, result);

		case 'index':
			var index = decoder.index;
			if (!(value instanceof Array))
			{
				return badPrimitive('an array', value);
			}
			if (index >= value.length)
			{
				return badPrimitive('a longer array. Need index ' + index + ' but there are only ' + value.length + ' entries', value);
			}

			var result = runHelp(decoder.decoder, value[index]);
			return (result.tag === 'ok') ? result : badIndex(index, result);

		case 'key-value':
			if (typeof value !== 'object' || value === null || value instanceof Array)
			{
				return badPrimitive('an object', value);
			}

			var keyValuePairs = _elm_lang$core$Native_List.Nil;
			for (var key in value)
			{
				var result = runHelp(decoder.decoder, value[key]);
				if (result.tag !== 'ok')
				{
					return badField(key, result);
				}
				var pair = _elm_lang$core$Native_Utils.Tuple2(key, result.value);
				keyValuePairs = _elm_lang$core$Native_List.Cons(pair, keyValuePairs);
			}
			return ok(keyValuePairs);

		case 'map-many':
			var answer = decoder.func;
			var decoders = decoder.decoders;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = runHelp(decoders[i], value);
				if (result.tag !== 'ok')
				{
					return result;
				}
				answer = answer(result.value);
			}
			return ok(answer);

		case 'andThen':
			var result = runHelp(decoder.decoder, value);
			return (result.tag !== 'ok')
				? result
				: runHelp(decoder.callback(result.value), value);

		case 'oneOf':
			var errors = [];
			var temp = decoder.decoders;
			while (temp.ctor !== '[]')
			{
				var result = runHelp(temp._0, value);

				if (result.tag === 'ok')
				{
					return result;
				}

				errors.push(result);

				temp = temp._1;
			}
			return badOneOf(errors);

		case 'fail':
			return bad(decoder.msg);

		case 'succeed':
			return ok(decoder.msg);
	}
}


// EQUALITY

function equality(a, b)
{
	if (a === b)
	{
		return true;
	}

	if (a.tag !== b.tag)
	{
		return false;
	}

	switch (a.tag)
	{
		case 'succeed':
		case 'fail':
			return a.msg === b.msg;

		case 'bool':
		case 'int':
		case 'float':
		case 'string':
		case 'value':
			return true;

		case 'null':
			return a.value === b.value;

		case 'list':
		case 'array':
		case 'maybe':
		case 'key-value':
			return equality(a.decoder, b.decoder);

		case 'field':
			return a.field === b.field && equality(a.decoder, b.decoder);

		case 'index':
			return a.index === b.index && equality(a.decoder, b.decoder);

		case 'map-many':
			if (a.func !== b.func)
			{
				return false;
			}
			return listEquality(a.decoders, b.decoders);

		case 'andThen':
			return a.callback === b.callback && equality(a.decoder, b.decoder);

		case 'oneOf':
			return listEquality(a.decoders, b.decoders);
	}
}

function listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

function encode(indentLevel, value)
{
	return JSON.stringify(value, null, indentLevel);
}

function identity(value)
{
	return value;
}

function encodeObject(keyValuePairs)
{
	var obj = {};
	while (keyValuePairs.ctor !== '[]')
	{
		var pair = keyValuePairs._0;
		obj[pair._0] = pair._1;
		keyValuePairs = keyValuePairs._1;
	}
	return obj;
}

return {
	encode: F2(encode),
	runOnString: F2(runOnString),
	run: F2(run),

	decodeNull: decodeNull,
	decodePrimitive: decodePrimitive,
	decodeContainer: F2(decodeContainer),

	decodeField: F2(decodeField),
	decodeIndex: F2(decodeIndex),

	map1: F2(map1),
	map2: F3(map2),
	map3: F4(map3),
	map4: F5(map4),
	map5: F6(map5),
	map6: F7(map6),
	map7: F8(map7),
	map8: F9(map8),
	decodeKeyValuePairs: decodeKeyValuePairs,

	andThen: F2(andThen),
	fail: fail,
	succeed: succeed,
	oneOf: oneOf,

	identity: identity,
	encodeNull: null,
	encodeArray: _elm_lang$core$Native_Array.toJSArray,
	encodeList: _elm_lang$core$Native_List.toArray,
	encodeObject: encodeObject,

	equality: equality
};

}();

var _elm_lang$core$Json_Encode$list = _elm_lang$core$Native_Json.encodeList;
var _elm_lang$core$Json_Encode$array = _elm_lang$core$Native_Json.encodeArray;
var _elm_lang$core$Json_Encode$object = _elm_lang$core$Native_Json.encodeObject;
var _elm_lang$core$Json_Encode$null = _elm_lang$core$Native_Json.encodeNull;
var _elm_lang$core$Json_Encode$bool = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$float = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$int = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$string = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$encode = _elm_lang$core$Native_Json.encode;
var _elm_lang$core$Json_Encode$Value = {ctor: 'Value'};

var _elm_lang$core$Json_Decode$null = _elm_lang$core$Native_Json.decodeNull;
var _elm_lang$core$Json_Decode$value = _elm_lang$core$Native_Json.decodePrimitive('value');
var _elm_lang$core$Json_Decode$andThen = _elm_lang$core$Native_Json.andThen;
var _elm_lang$core$Json_Decode$fail = _elm_lang$core$Native_Json.fail;
var _elm_lang$core$Json_Decode$succeed = _elm_lang$core$Native_Json.succeed;
var _elm_lang$core$Json_Decode$lazy = function (thunk) {
	return A2(
		_elm_lang$core$Json_Decode$andThen,
		thunk,
		_elm_lang$core$Json_Decode$succeed(
			{ctor: '_Tuple0'}));
};
var _elm_lang$core$Json_Decode$decodeValue = _elm_lang$core$Native_Json.run;
var _elm_lang$core$Json_Decode$decodeString = _elm_lang$core$Native_Json.runOnString;
var _elm_lang$core$Json_Decode$map8 = _elm_lang$core$Native_Json.map8;
var _elm_lang$core$Json_Decode$map7 = _elm_lang$core$Native_Json.map7;
var _elm_lang$core$Json_Decode$map6 = _elm_lang$core$Native_Json.map6;
var _elm_lang$core$Json_Decode$map5 = _elm_lang$core$Native_Json.map5;
var _elm_lang$core$Json_Decode$map4 = _elm_lang$core$Native_Json.map4;
var _elm_lang$core$Json_Decode$map3 = _elm_lang$core$Native_Json.map3;
var _elm_lang$core$Json_Decode$map2 = _elm_lang$core$Native_Json.map2;
var _elm_lang$core$Json_Decode$map = _elm_lang$core$Native_Json.map1;
var _elm_lang$core$Json_Decode$oneOf = _elm_lang$core$Native_Json.oneOf;
var _elm_lang$core$Json_Decode$maybe = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'maybe', decoder);
};
var _elm_lang$core$Json_Decode$index = _elm_lang$core$Native_Json.decodeIndex;
var _elm_lang$core$Json_Decode$field = _elm_lang$core$Native_Json.decodeField;
var _elm_lang$core$Json_Decode$at = F2(
	function (fields, decoder) {
		return A3(_elm_lang$core$List$foldr, _elm_lang$core$Json_Decode$field, decoder, fields);
	});
var _elm_lang$core$Json_Decode$keyValuePairs = _elm_lang$core$Native_Json.decodeKeyValuePairs;
var _elm_lang$core$Json_Decode$dict = function (decoder) {
	return A2(
		_elm_lang$core$Json_Decode$map,
		_elm_lang$core$Dict$fromList,
		_elm_lang$core$Json_Decode$keyValuePairs(decoder));
};
var _elm_lang$core$Json_Decode$array = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'array', decoder);
};
var _elm_lang$core$Json_Decode$list = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'list', decoder);
};
var _elm_lang$core$Json_Decode$nullable = function (decoder) {
	return _elm_lang$core$Json_Decode$oneOf(
		{
			ctor: '::',
			_0: _elm_lang$core$Json_Decode$null(_elm_lang$core$Maybe$Nothing),
			_1: {
				ctor: '::',
				_0: A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Maybe$Just, decoder),
				_1: {ctor: '[]'}
			}
		});
};
var _elm_lang$core$Json_Decode$float = _elm_lang$core$Native_Json.decodePrimitive('float');
var _elm_lang$core$Json_Decode$int = _elm_lang$core$Native_Json.decodePrimitive('int');
var _elm_lang$core$Json_Decode$bool = _elm_lang$core$Native_Json.decodePrimitive('bool');
var _elm_lang$core$Json_Decode$string = _elm_lang$core$Native_Json.decodePrimitive('string');
var _elm_lang$core$Json_Decode$Decoder = {ctor: 'Decoder'};

var _elm_lang$core$Process$kill = _elm_lang$core$Native_Scheduler.kill;
var _elm_lang$core$Process$sleep = _elm_lang$core$Native_Scheduler.sleep;
var _elm_lang$core$Process$spawn = _elm_lang$core$Native_Scheduler.spawn;

var _elm_lang$dom$Native_Dom = function() {

var fakeNode = {
	addEventListener: function() {},
	removeEventListener: function() {}
};

var onDocument = on(typeof document !== 'undefined' ? document : fakeNode);
var onWindow = on(typeof window !== 'undefined' ? window : fakeNode);

function on(node)
{
	return function(eventName, decoder, toTask)
	{
		return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {

			function performTask(event)
			{
				var result = A2(_elm_lang$core$Json_Decode$decodeValue, decoder, event);
				if (result.ctor === 'Ok')
				{
					_elm_lang$core$Native_Scheduler.rawSpawn(toTask(result._0));
				}
			}

			node.addEventListener(eventName, performTask);

			return function()
			{
				node.removeEventListener(eventName, performTask);
			};
		});
	};
}

var rAF = typeof requestAnimationFrame !== 'undefined'
	? requestAnimationFrame
	: function(callback) { callback(); };

function withNode(id, doStuff)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		rAF(function()
		{
			var node = document.getElementById(id);
			if (node === null)
			{
				callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'NotFound', _0: id }));
				return;
			}
			callback(_elm_lang$core$Native_Scheduler.succeed(doStuff(node)));
		});
	});
}


// FOCUS

function focus(id)
{
	return withNode(id, function(node) {
		node.focus();
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function blur(id)
{
	return withNode(id, function(node) {
		node.blur();
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}


// SCROLLING

function getScrollTop(id)
{
	return withNode(id, function(node) {
		return node.scrollTop;
	});
}

function setScrollTop(id, desiredScrollTop)
{
	return withNode(id, function(node) {
		node.scrollTop = desiredScrollTop;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function toBottom(id)
{
	return withNode(id, function(node) {
		node.scrollTop = node.scrollHeight;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function getScrollLeft(id)
{
	return withNode(id, function(node) {
		return node.scrollLeft;
	});
}

function setScrollLeft(id, desiredScrollLeft)
{
	return withNode(id, function(node) {
		node.scrollLeft = desiredScrollLeft;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function toRight(id)
{
	return withNode(id, function(node) {
		node.scrollLeft = node.scrollWidth;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}


// SIZE

function width(options, id)
{
	return withNode(id, function(node) {
		switch (options.ctor)
		{
			case 'Content':
				return node.scrollWidth;
			case 'VisibleContent':
				return node.clientWidth;
			case 'VisibleContentWithBorders':
				return node.offsetWidth;
			case 'VisibleContentWithBordersAndMargins':
				var rect = node.getBoundingClientRect();
				return rect.right - rect.left;
		}
	});
}

function height(options, id)
{
	return withNode(id, function(node) {
		switch (options.ctor)
		{
			case 'Content':
				return node.scrollHeight;
			case 'VisibleContent':
				return node.clientHeight;
			case 'VisibleContentWithBorders':
				return node.offsetHeight;
			case 'VisibleContentWithBordersAndMargins':
				var rect = node.getBoundingClientRect();
				return rect.bottom - rect.top;
		}
	});
}

return {
	onDocument: F3(onDocument),
	onWindow: F3(onWindow),

	focus: focus,
	blur: blur,

	getScrollTop: getScrollTop,
	setScrollTop: F2(setScrollTop),
	getScrollLeft: getScrollLeft,
	setScrollLeft: F2(setScrollLeft),
	toBottom: toBottom,
	toRight: toRight,

	height: F2(height),
	width: F2(width)
};

}();

var _elm_lang$dom$Dom_LowLevel$onWindow = _elm_lang$dom$Native_Dom.onWindow;
var _elm_lang$dom$Dom_LowLevel$onDocument = _elm_lang$dom$Native_Dom.onDocument;

var _elm_lang$virtual_dom$VirtualDom_Debug$wrap;
var _elm_lang$virtual_dom$VirtualDom_Debug$wrapWithFlags;

var _elm_lang$virtual_dom$Native_VirtualDom = function() {

var STYLE_KEY = 'STYLE';
var EVENT_KEY = 'EVENT';
var ATTR_KEY = 'ATTR';
var ATTR_NS_KEY = 'ATTR_NS';

var localDoc = typeof document !== 'undefined' ? document : {};


////////////  VIRTUAL DOM NODES  ////////////


function text(string)
{
	return {
		type: 'text',
		text: string
	};
}


function node(tag)
{
	return F2(function(factList, kidList) {
		return nodeHelp(tag, factList, kidList);
	});
}


function nodeHelp(tag, factList, kidList)
{
	var organized = organizeFacts(factList);
	var namespace = organized.namespace;
	var facts = organized.facts;

	var children = [];
	var descendantsCount = 0;
	while (kidList.ctor !== '[]')
	{
		var kid = kidList._0;
		descendantsCount += (kid.descendantsCount || 0);
		children.push(kid);
		kidList = kidList._1;
	}
	descendantsCount += children.length;

	return {
		type: 'node',
		tag: tag,
		facts: facts,
		children: children,
		namespace: namespace,
		descendantsCount: descendantsCount
	};
}


function keyedNode(tag, factList, kidList)
{
	var organized = organizeFacts(factList);
	var namespace = organized.namespace;
	var facts = organized.facts;

	var children = [];
	var descendantsCount = 0;
	while (kidList.ctor !== '[]')
	{
		var kid = kidList._0;
		descendantsCount += (kid._1.descendantsCount || 0);
		children.push(kid);
		kidList = kidList._1;
	}
	descendantsCount += children.length;

	return {
		type: 'keyed-node',
		tag: tag,
		facts: facts,
		children: children,
		namespace: namespace,
		descendantsCount: descendantsCount
	};
}


function custom(factList, model, impl)
{
	var facts = organizeFacts(factList).facts;

	return {
		type: 'custom',
		facts: facts,
		model: model,
		impl: impl
	};
}


function map(tagger, node)
{
	return {
		type: 'tagger',
		tagger: tagger,
		node: node,
		descendantsCount: 1 + (node.descendantsCount || 0)
	};
}


function thunk(func, args, thunk)
{
	return {
		type: 'thunk',
		func: func,
		args: args,
		thunk: thunk,
		node: undefined
	};
}

function lazy(fn, a)
{
	return thunk(fn, [a], function() {
		return fn(a);
	});
}

function lazy2(fn, a, b)
{
	return thunk(fn, [a,b], function() {
		return A2(fn, a, b);
	});
}

function lazy3(fn, a, b, c)
{
	return thunk(fn, [a,b,c], function() {
		return A3(fn, a, b, c);
	});
}



// FACTS


function organizeFacts(factList)
{
	var namespace, facts = {};

	while (factList.ctor !== '[]')
	{
		var entry = factList._0;
		var key = entry.key;

		if (key === ATTR_KEY || key === ATTR_NS_KEY || key === EVENT_KEY)
		{
			var subFacts = facts[key] || {};
			subFacts[entry.realKey] = entry.value;
			facts[key] = subFacts;
		}
		else if (key === STYLE_KEY)
		{
			var styles = facts[key] || {};
			var styleList = entry.value;
			while (styleList.ctor !== '[]')
			{
				var style = styleList._0;
				styles[style._0] = style._1;
				styleList = styleList._1;
			}
			facts[key] = styles;
		}
		else if (key === 'namespace')
		{
			namespace = entry.value;
		}
		else if (key === 'className')
		{
			var classes = facts[key];
			facts[key] = typeof classes === 'undefined'
				? entry.value
				: classes + ' ' + entry.value;
		}
 		else
		{
			facts[key] = entry.value;
		}
		factList = factList._1;
	}

	return {
		facts: facts,
		namespace: namespace
	};
}



////////////  PROPERTIES AND ATTRIBUTES  ////////////


function style(value)
{
	return {
		key: STYLE_KEY,
		value: value
	};
}


function property(key, value)
{
	return {
		key: key,
		value: value
	};
}


function attribute(key, value)
{
	return {
		key: ATTR_KEY,
		realKey: key,
		value: value
	};
}


function attributeNS(namespace, key, value)
{
	return {
		key: ATTR_NS_KEY,
		realKey: key,
		value: {
			value: value,
			namespace: namespace
		}
	};
}


function on(name, options, decoder)
{
	return {
		key: EVENT_KEY,
		realKey: name,
		value: {
			options: options,
			decoder: decoder
		}
	};
}


function equalEvents(a, b)
{
	if (a.options !== b.options)
	{
		if (a.options.stopPropagation !== b.options.stopPropagation || a.options.preventDefault !== b.options.preventDefault)
		{
			return false;
		}
	}
	return _elm_lang$core$Native_Json.equality(a.decoder, b.decoder);
}


function mapProperty(func, property)
{
	if (property.key !== EVENT_KEY)
	{
		return property;
	}
	return on(
		property.realKey,
		property.value.options,
		A2(_elm_lang$core$Json_Decode$map, func, property.value.decoder)
	);
}


////////////  RENDER  ////////////


function render(vNode, eventNode)
{
	switch (vNode.type)
	{
		case 'thunk':
			if (!vNode.node)
			{
				vNode.node = vNode.thunk();
			}
			return render(vNode.node, eventNode);

		case 'tagger':
			var subNode = vNode.node;
			var tagger = vNode.tagger;

			while (subNode.type === 'tagger')
			{
				typeof tagger !== 'object'
					? tagger = [tagger, subNode.tagger]
					: tagger.push(subNode.tagger);

				subNode = subNode.node;
			}

			var subEventRoot = { tagger: tagger, parent: eventNode };
			var domNode = render(subNode, subEventRoot);
			domNode.elm_event_node_ref = subEventRoot;
			return domNode;

		case 'text':
			return localDoc.createTextNode(vNode.text);

		case 'node':
			var domNode = vNode.namespace
				? localDoc.createElementNS(vNode.namespace, vNode.tag)
				: localDoc.createElement(vNode.tag);

			applyFacts(domNode, eventNode, vNode.facts);

			var children = vNode.children;

			for (var i = 0; i < children.length; i++)
			{
				domNode.appendChild(render(children[i], eventNode));
			}

			return domNode;

		case 'keyed-node':
			var domNode = vNode.namespace
				? localDoc.createElementNS(vNode.namespace, vNode.tag)
				: localDoc.createElement(vNode.tag);

			applyFacts(domNode, eventNode, vNode.facts);

			var children = vNode.children;

			for (var i = 0; i < children.length; i++)
			{
				domNode.appendChild(render(children[i]._1, eventNode));
			}

			return domNode;

		case 'custom':
			var domNode = vNode.impl.render(vNode.model);
			applyFacts(domNode, eventNode, vNode.facts);
			return domNode;
	}
}



////////////  APPLY FACTS  ////////////


function applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		switch (key)
		{
			case STYLE_KEY:
				applyStyles(domNode, value);
				break;

			case EVENT_KEY:
				applyEvents(domNode, eventNode, value);
				break;

			case ATTR_KEY:
				applyAttrs(domNode, value);
				break;

			case ATTR_NS_KEY:
				applyAttrsNS(domNode, value);
				break;

			case 'value':
				if (domNode[key] !== value)
				{
					domNode[key] = value;
				}
				break;

			default:
				domNode[key] = value;
				break;
		}
	}
}

function applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}

function applyEvents(domNode, eventNode, events)
{
	var allHandlers = domNode.elm_handlers || {};

	for (var key in events)
	{
		var handler = allHandlers[key];
		var value = events[key];

		if (typeof value === 'undefined')
		{
			domNode.removeEventListener(key, handler);
			allHandlers[key] = undefined;
		}
		else if (typeof handler === 'undefined')
		{
			var handler = makeEventHandler(eventNode, value);
			domNode.addEventListener(key, handler);
			allHandlers[key] = handler;
		}
		else
		{
			handler.info = value;
		}
	}

	domNode.elm_handlers = allHandlers;
}

function makeEventHandler(eventNode, info)
{
	function eventHandler(event)
	{
		var info = eventHandler.info;

		var value = A2(_elm_lang$core$Native_Json.run, info.decoder, event);

		if (value.ctor === 'Ok')
		{
			var options = info.options;
			if (options.stopPropagation)
			{
				event.stopPropagation();
			}
			if (options.preventDefault)
			{
				event.preventDefault();
			}

			var message = value._0;

			var currentEventNode = eventNode;
			while (currentEventNode)
			{
				var tagger = currentEventNode.tagger;
				if (typeof tagger === 'function')
				{
					message = tagger(message);
				}
				else
				{
					for (var i = tagger.length; i--; )
					{
						message = tagger[i](message);
					}
				}
				currentEventNode = currentEventNode.parent;
			}
		}
	};

	eventHandler.info = info;

	return eventHandler;
}

function applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		if (typeof value === 'undefined')
		{
			domNode.removeAttribute(key);
		}
		else
		{
			domNode.setAttribute(key, value);
		}
	}
}

function applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.namespace;
		var value = pair.value;

		if (typeof value === 'undefined')
		{
			domNode.removeAttributeNS(namespace, key);
		}
		else
		{
			domNode.setAttributeNS(namespace, key, value);
		}
	}
}



////////////  DIFF  ////////////


function diff(a, b)
{
	var patches = [];
	diffHelp(a, b, patches, 0);
	return patches;
}


function makePatch(type, index, data)
{
	return {
		index: index,
		type: type,
		data: data,
		domNode: undefined,
		eventNode: undefined
	};
}


function diffHelp(a, b, patches, index)
{
	if (a === b)
	{
		return;
	}

	var aType = a.type;
	var bType = b.type;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (aType !== bType)
	{
		patches.push(makePatch('p-redraw', index, b));
		return;
	}

	// Now we know that both nodes are the same type.
	switch (bType)
	{
		case 'thunk':
			var aArgs = a.args;
			var bArgs = b.args;
			var i = aArgs.length;
			var same = a.func === b.func && i === bArgs.length;
			while (same && i--)
			{
				same = aArgs[i] === bArgs[i];
			}
			if (same)
			{
				b.node = a.node;
				return;
			}
			b.node = b.thunk();
			var subPatches = [];
			diffHelp(a.node, b.node, subPatches, 0);
			if (subPatches.length > 0)
			{
				patches.push(makePatch('p-thunk', index, subPatches));
			}
			return;

		case 'tagger':
			// gather nested taggers
			var aTaggers = a.tagger;
			var bTaggers = b.tagger;
			var nesting = false;

			var aSubNode = a.node;
			while (aSubNode.type === 'tagger')
			{
				nesting = true;

				typeof aTaggers !== 'object'
					? aTaggers = [aTaggers, aSubNode.tagger]
					: aTaggers.push(aSubNode.tagger);

				aSubNode = aSubNode.node;
			}

			var bSubNode = b.node;
			while (bSubNode.type === 'tagger')
			{
				nesting = true;

				typeof bTaggers !== 'object'
					? bTaggers = [bTaggers, bSubNode.tagger]
					: bTaggers.push(bSubNode.tagger);

				bSubNode = bSubNode.node;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && aTaggers.length !== bTaggers.length)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !pairwiseRefEqual(aTaggers, bTaggers) : aTaggers !== bTaggers)
			{
				patches.push(makePatch('p-tagger', index, bTaggers));
			}

			// diff everything below the taggers
			diffHelp(aSubNode, bSubNode, patches, index + 1);
			return;

		case 'text':
			if (a.text !== b.text)
			{
				patches.push(makePatch('p-text', index, b.text));
				return;
			}

			return;

		case 'node':
			// Bail if obvious indicators have changed. Implies more serious
			// structural changes such that it's not worth it to diff.
			if (a.tag !== b.tag || a.namespace !== b.namespace)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);

			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			diffChildren(a, b, patches, index);
			return;

		case 'keyed-node':
			// Bail if obvious indicators have changed. Implies more serious
			// structural changes such that it's not worth it to diff.
			if (a.tag !== b.tag || a.namespace !== b.namespace)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);

			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			diffKeyedChildren(a, b, patches, index);
			return;

		case 'custom':
			if (a.impl !== b.impl)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);
			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			var patch = b.impl.diff(a,b);
			if (patch)
			{
				patches.push(makePatch('p-custom', index, patch));
				return;
			}

			return;
	}
}


// assumes the incoming arrays are the same length
function pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function diffFacts(a, b, category)
{
	var diff;

	// look for changes and removals
	for (var aKey in a)
	{
		if (aKey === STYLE_KEY || aKey === EVENT_KEY || aKey === ATTR_KEY || aKey === ATTR_NS_KEY)
		{
			var subDiff = diffFacts(a[aKey], b[aKey] || {}, aKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[aKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(aKey in b))
		{
			diff = diff || {};
			diff[aKey] =
				(typeof category === 'undefined')
					? (typeof a[aKey] === 'string' ? '' : null)
					:
				(category === STYLE_KEY)
					? ''
					:
				(category === EVENT_KEY || category === ATTR_KEY)
					? undefined
					:
				{ namespace: a[aKey].namespace, value: undefined };

			continue;
		}

		var aValue = a[aKey];
		var bValue = b[aKey];

		// reference equal, so don't worry about it
		if (aValue === bValue && aKey !== 'value'
			|| category === EVENT_KEY && equalEvents(aValue, bValue))
		{
			continue;
		}

		diff = diff || {};
		diff[aKey] = bValue;
	}

	// add new stuff
	for (var bKey in b)
	{
		if (!(bKey in a))
		{
			diff = diff || {};
			diff[bKey] = b[bKey];
		}
	}

	return diff;
}


function diffChildren(aParent, bParent, patches, rootIndex)
{
	var aChildren = aParent.children;
	var bChildren = bParent.children;

	var aLen = aChildren.length;
	var bLen = bChildren.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (aLen > bLen)
	{
		patches.push(makePatch('p-remove-last', rootIndex, aLen - bLen));
	}
	else if (aLen < bLen)
	{
		patches.push(makePatch('p-append', rootIndex, bChildren.slice(aLen)));
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	var index = rootIndex;
	var minLen = aLen < bLen ? aLen : bLen;
	for (var i = 0; i < minLen; i++)
	{
		index++;
		var aChild = aChildren[i];
		diffHelp(aChild, bChildren[i], patches, index);
		index += aChild.descendantsCount || 0;
	}
}



////////////  KEYED DIFF  ////////////


function diffKeyedChildren(aParent, bParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var aChildren = aParent.children;
	var bChildren = bParent.children;
	var aLen = aChildren.length;
	var bLen = bChildren.length;
	var aIndex = 0;
	var bIndex = 0;

	var index = rootIndex;

	while (aIndex < aLen && bIndex < bLen)
	{
		var a = aChildren[aIndex];
		var b = bChildren[bIndex];

		var aKey = a._0;
		var bKey = b._0;
		var aNode = a._1;
		var bNode = b._1;

		// check if keys match

		if (aKey === bKey)
		{
			index++;
			diffHelp(aNode, bNode, localPatches, index);
			index += aNode.descendantsCount || 0;

			aIndex++;
			bIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var aLookAhead = aIndex + 1 < aLen;
		var bLookAhead = bIndex + 1 < bLen;

		if (aLookAhead)
		{
			var aNext = aChildren[aIndex + 1];
			var aNextKey = aNext._0;
			var aNextNode = aNext._1;
			var oldMatch = bKey === aNextKey;
		}

		if (bLookAhead)
		{
			var bNext = bChildren[bIndex + 1];
			var bNextKey = bNext._0;
			var bNextNode = bNext._1;
			var newMatch = aKey === bNextKey;
		}


		// swap a and b
		if (aLookAhead && bLookAhead && newMatch && oldMatch)
		{
			index++;
			diffHelp(aNode, bNextNode, localPatches, index);
			insertNode(changes, localPatches, aKey, bNode, bIndex, inserts);
			index += aNode.descendantsCount || 0;

			index++;
			removeNode(changes, localPatches, aKey, aNextNode, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 2;
			continue;
		}

		// insert b
		if (bLookAhead && newMatch)
		{
			index++;
			insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
			diffHelp(aNode, bNextNode, localPatches, index);
			index += aNode.descendantsCount || 0;

			aIndex += 1;
			bIndex += 2;
			continue;
		}

		// remove a
		if (aLookAhead && oldMatch)
		{
			index++;
			removeNode(changes, localPatches, aKey, aNode, index);
			index += aNode.descendantsCount || 0;

			index++;
			diffHelp(aNextNode, bNode, localPatches, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 1;
			continue;
		}

		// remove a, insert b
		if (aLookAhead && bLookAhead && aNextKey === bNextKey)
		{
			index++;
			removeNode(changes, localPatches, aKey, aNode, index);
			insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
			index += aNode.descendantsCount || 0;

			index++;
			diffHelp(aNextNode, bNextNode, localPatches, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (aIndex < aLen)
	{
		index++;
		var a = aChildren[aIndex];
		var aNode = a._1;
		removeNode(changes, localPatches, a._0, aNode, index);
		index += aNode.descendantsCount || 0;
		aIndex++;
	}

	var endInserts;
	while (bIndex < bLen)
	{
		endInserts = endInserts || [];
		var b = bChildren[bIndex];
		insertNode(changes, localPatches, b._0, b._1, undefined, endInserts);
		bIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || typeof endInserts !== 'undefined')
	{
		patches.push(makePatch('p-reorder', rootIndex, {
			patches: localPatches,
			inserts: inserts,
			endInserts: endInserts
		}));
	}
}



////////////  CHANGES FROM KEYED DIFF  ////////////


var POSTFIX = '_elmW6BL';


function insertNode(changes, localPatches, key, vnode, bIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (typeof entry === 'undefined')
	{
		entry = {
			tag: 'insert',
			vnode: vnode,
			index: bIndex,
			data: undefined
		};

		inserts.push({ index: bIndex, entry: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.tag === 'remove')
	{
		inserts.push({ index: bIndex, entry: entry });

		entry.tag = 'move';
		var subPatches = [];
		diffHelp(entry.vnode, vnode, subPatches, entry.index);
		entry.index = bIndex;
		entry.data.data = {
			patches: subPatches,
			entry: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	insertNode(changes, localPatches, key + POSTFIX, vnode, bIndex, inserts);
}


function removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (typeof entry === 'undefined')
	{
		var patch = makePatch('p-remove', index, undefined);
		localPatches.push(patch);

		changes[key] = {
			tag: 'remove',
			vnode: vnode,
			index: index,
			data: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.tag === 'insert')
	{
		entry.tag = 'move';
		var subPatches = [];
		diffHelp(vnode, entry.vnode, subPatches, index);

		var patch = makePatch('p-remove', index, {
			patches: subPatches,
			entry: entry
		});
		localPatches.push(patch);

		return;
	}

	// this key has already been removed or moved, a duplicate!
	removeNode(changes, localPatches, key + POSTFIX, vnode, index);
}



////////////  ADD DOM NODES  ////////////
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function addDomNodes(domNode, vNode, patches, eventNode)
{
	addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.descendantsCount, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.index;

	while (index === low)
	{
		var patchType = patch.type;

		if (patchType === 'p-thunk')
		{
			addDomNodes(domNode, vNode.node, patch.data, eventNode);
		}
		else if (patchType === 'p-reorder')
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;

			var subPatches = patch.data.patches;
			if (subPatches.length > 0)
			{
				addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 'p-remove')
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;

			var data = patch.data;
			if (typeof data !== 'undefined')
			{
				data.entry.data = domNode;
				var subPatches = data.patches;
				if (subPatches.length > 0)
				{
					addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.index) > high)
		{
			return i;
		}
	}

	switch (vNode.type)
	{
		case 'tagger':
			var subNode = vNode.node;

			while (subNode.type === "tagger")
			{
				subNode = subNode.node;
			}

			return addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);

		case 'node':
			var vChildren = vNode.children;
			var childNodes = domNode.childNodes;
			for (var j = 0; j < vChildren.length; j++)
			{
				low++;
				var vChild = vChildren[j];
				var nextLow = low + (vChild.descendantsCount || 0);
				if (low <= index && index <= nextLow)
				{
					i = addDomNodesHelp(childNodes[j], vChild, patches, i, low, nextLow, eventNode);
					if (!(patch = patches[i]) || (index = patch.index) > high)
					{
						return i;
					}
				}
				low = nextLow;
			}
			return i;

		case 'keyed-node':
			var vChildren = vNode.children;
			var childNodes = domNode.childNodes;
			for (var j = 0; j < vChildren.length; j++)
			{
				low++;
				var vChild = vChildren[j]._1;
				var nextLow = low + (vChild.descendantsCount || 0);
				if (low <= index && index <= nextLow)
				{
					i = addDomNodesHelp(childNodes[j], vChild, patches, i, low, nextLow, eventNode);
					if (!(patch = patches[i]) || (index = patch.index) > high)
					{
						return i;
					}
				}
				low = nextLow;
			}
			return i;

		case 'text':
		case 'thunk':
			throw new Error('should never traverse `text` or `thunk` nodes like this');
	}
}



////////////  APPLY PATCHES  ////////////


function applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return applyPatchesHelp(rootDomNode, patches);
}

function applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.domNode
		var newNode = applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function applyPatch(domNode, patch)
{
	switch (patch.type)
	{
		case 'p-redraw':
			return applyPatchRedraw(domNode, patch.data, patch.eventNode);

		case 'p-facts':
			applyFacts(domNode, patch.eventNode, patch.data);
			return domNode;

		case 'p-text':
			domNode.replaceData(0, domNode.length, patch.data);
			return domNode;

		case 'p-thunk':
			return applyPatchesHelp(domNode, patch.data);

		case 'p-tagger':
			if (typeof domNode.elm_event_node_ref !== 'undefined')
			{
				domNode.elm_event_node_ref.tagger = patch.data;
			}
			else
			{
				domNode.elm_event_node_ref = { tagger: patch.data, parent: patch.eventNode };
			}
			return domNode;

		case 'p-remove-last':
			var i = patch.data;
			while (i--)
			{
				domNode.removeChild(domNode.lastChild);
			}
			return domNode;

		case 'p-append':
			var newNodes = patch.data;
			for (var i = 0; i < newNodes.length; i++)
			{
				domNode.appendChild(render(newNodes[i], patch.eventNode));
			}
			return domNode;

		case 'p-remove':
			var data = patch.data;
			if (typeof data === 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.entry;
			if (typeof entry.index !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.data = applyPatchesHelp(domNode, data.patches);
			return domNode;

		case 'p-reorder':
			return applyPatchReorder(domNode, patch);

		case 'p-custom':
			var impl = patch.data;
			return impl.applyPatch(domNode, impl.data);

		default:
			throw new Error('Ran into an unknown patch!');
	}
}


function applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = render(vNode, eventNode);

	if (typeof newNode.elm_event_node_ref === 'undefined')
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function applyPatchReorder(domNode, patch)
{
	var data = patch.data;

	// remove end inserts
	var frag = applyPatchReorderEndInsertsHelp(data.endInserts, patch);

	// removals
	domNode = applyPatchesHelp(domNode, data.patches);

	// inserts
	var inserts = data.inserts;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.entry;
		var node = entry.tag === 'move'
			? entry.data
			: render(entry.vnode, patch.eventNode);
		domNode.insertBefore(node, domNode.childNodes[insert.index]);
	}

	// add end inserts
	if (typeof frag !== 'undefined')
	{
		domNode.appendChild(frag);
	}

	return domNode;
}


function applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (typeof endInserts === 'undefined')
	{
		return;
	}

	var frag = localDoc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.entry;
		frag.appendChild(entry.tag === 'move'
			? entry.data
			: render(entry.vnode, patch.eventNode)
		);
	}
	return frag;
}


// PROGRAMS

var program = makeProgram(checkNoFlags);
var programWithFlags = makeProgram(checkYesFlags);

function makeProgram(flagChecker)
{
	return F2(function(debugWrap, impl)
	{
		return function(flagDecoder)
		{
			return function(object, moduleName, debugMetadata)
			{
				var checker = flagChecker(flagDecoder, moduleName);
				if (typeof debugMetadata === 'undefined')
				{
					normalSetup(impl, object, moduleName, checker);
				}
				else
				{
					debugSetup(A2(debugWrap, debugMetadata, impl), object, moduleName, checker);
				}
			};
		};
	});
}

function staticProgram(vNode)
{
	var nothing = _elm_lang$core$Native_Utils.Tuple2(
		_elm_lang$core$Native_Utils.Tuple0,
		_elm_lang$core$Platform_Cmd$none
	);
	return A2(program, _elm_lang$virtual_dom$VirtualDom_Debug$wrap, {
		init: nothing,
		view: function() { return vNode; },
		update: F2(function() { return nothing; }),
		subscriptions: function() { return _elm_lang$core$Platform_Sub$none; }
	})();
}


// FLAG CHECKERS

function checkNoFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flags === 'undefined')
		{
			return init;
		}

		var errorMessage =
			'The `' + moduleName + '` module does not need flags.\n'
			+ 'Initialize it with no arguments and you should be all set!';

		crash(errorMessage, domNode);
	};
}

function checkYesFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flagDecoder === 'undefined')
		{
			var errorMessage =
				'Are you trying to sneak a Never value into Elm? Trickster!\n'
				+ 'It looks like ' + moduleName + '.main is defined with `programWithFlags` but has type `Program Never`.\n'
				+ 'Use `program` instead if you do not want flags.'

			crash(errorMessage, domNode);
		}

		var result = A2(_elm_lang$core$Native_Json.run, flagDecoder, flags);
		if (result.ctor === 'Ok')
		{
			return init(result._0);
		}

		var errorMessage =
			'Trying to initialize the `' + moduleName + '` module with an unexpected flag.\n'
			+ 'I tried to convert it to an Elm value, but ran into this problem:\n\n'
			+ result._0;

		crash(errorMessage, domNode);
	};
}

function crash(errorMessage, domNode)
{
	if (domNode)
	{
		domNode.innerHTML =
			'<div style="padding-left:1em;">'
			+ '<h2 style="font-weight:normal;"><b>Oops!</b> Something went wrong when starting your Elm program.</h2>'
			+ '<pre style="padding-left:1em;">' + errorMessage + '</pre>'
			+ '</div>';
	}

	throw new Error(errorMessage);
}


//  NORMAL SETUP

function normalSetup(impl, object, moduleName, flagChecker)
{
	object['embed'] = function embed(node, flags)
	{
		while (node.lastChild)
		{
			node.removeChild(node.lastChild);
		}

		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, node),
			impl.update,
			impl.subscriptions,
			normalRenderer(node, impl.view)
		);
	};

	object['fullscreen'] = function fullscreen(flags)
	{
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, document.body),
			impl.update,
			impl.subscriptions,
			normalRenderer(document.body, impl.view)
		);
	};
}

function normalRenderer(parentNode, view)
{
	return function(tagger, initialModel)
	{
		var eventNode = { tagger: tagger, parent: undefined };
		var initialVirtualNode = view(initialModel);
		var domNode = render(initialVirtualNode, eventNode);
		parentNode.appendChild(domNode);
		return makeStepper(domNode, view, initialVirtualNode, eventNode);
	};
}


// STEPPER

var rAF =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { setTimeout(callback, 1000 / 60); };

function makeStepper(domNode, view, initialVirtualNode, eventNode)
{
	var state = 'NO_REQUEST';
	var currNode = initialVirtualNode;
	var nextModel;

	function updateIfNeeded()
	{
		switch (state)
		{
			case 'NO_REQUEST':
				throw new Error(
					'Unexpected draw callback.\n' +
					'Please report this to <https://github.com/elm-lang/virtual-dom/issues>.'
				);

			case 'PENDING_REQUEST':
				rAF(updateIfNeeded);
				state = 'EXTRA_REQUEST';

				var nextNode = view(nextModel);
				var patches = diff(currNode, nextNode);
				domNode = applyPatches(domNode, currNode, patches, eventNode);
				currNode = nextNode;

				return;

			case 'EXTRA_REQUEST':
				state = 'NO_REQUEST';
				return;
		}
	}

	return function stepper(model)
	{
		if (state === 'NO_REQUEST')
		{
			rAF(updateIfNeeded);
		}
		state = 'PENDING_REQUEST';
		nextModel = model;
	};
}


// DEBUG SETUP

function debugSetup(impl, object, moduleName, flagChecker)
{
	object['fullscreen'] = function fullscreen(flags)
	{
		var popoutRef = { doc: undefined };
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, document.body),
			impl.update(scrollTask(popoutRef)),
			impl.subscriptions,
			debugRenderer(moduleName, document.body, popoutRef, impl.view, impl.viewIn, impl.viewOut)
		);
	};

	object['embed'] = function fullscreen(node, flags)
	{
		var popoutRef = { doc: undefined };
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, node),
			impl.update(scrollTask(popoutRef)),
			impl.subscriptions,
			debugRenderer(moduleName, node, popoutRef, impl.view, impl.viewIn, impl.viewOut)
		);
	};
}

function scrollTask(popoutRef)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var doc = popoutRef.doc;
		if (doc)
		{
			var msgs = doc.getElementsByClassName('debugger-sidebar-messages')[0];
			if (msgs)
			{
				msgs.scrollTop = msgs.scrollHeight;
			}
		}
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}


function debugRenderer(moduleName, parentNode, popoutRef, view, viewIn, viewOut)
{
	return function(tagger, initialModel)
	{
		var appEventNode = { tagger: tagger, parent: undefined };
		var eventNode = { tagger: tagger, parent: undefined };

		// make normal stepper
		var appVirtualNode = view(initialModel);
		var appNode = render(appVirtualNode, appEventNode);
		parentNode.appendChild(appNode);
		var appStepper = makeStepper(appNode, view, appVirtualNode, appEventNode);

		// make overlay stepper
		var overVirtualNode = viewIn(initialModel)._1;
		var overNode = render(overVirtualNode, eventNode);
		parentNode.appendChild(overNode);
		var wrappedViewIn = wrapViewIn(appEventNode, overNode, viewIn);
		var overStepper = makeStepper(overNode, wrappedViewIn, overVirtualNode, eventNode);

		// make debugger stepper
		var debugStepper = makeDebugStepper(initialModel, viewOut, eventNode, parentNode, moduleName, popoutRef);

		return function stepper(model)
		{
			appStepper(model);
			overStepper(model);
			debugStepper(model);
		}
	};
}

function makeDebugStepper(initialModel, view, eventNode, parentNode, moduleName, popoutRef)
{
	var curr;
	var domNode;

	return function stepper(model)
	{
		if (!model.isDebuggerOpen)
		{
			return;
		}

		if (!popoutRef.doc)
		{
			curr = view(model);
			domNode = openDebugWindow(moduleName, popoutRef, curr, eventNode);
			return;
		}

		// switch to document of popout
		localDoc = popoutRef.doc;

		var next = view(model);
		var patches = diff(curr, next);
		domNode = applyPatches(domNode, curr, patches, eventNode);
		curr = next;

		// switch back to normal document
		localDoc = document;
	};
}

function openDebugWindow(moduleName, popoutRef, virtualNode, eventNode)
{
	var w = 900;
	var h = 360;
	var x = screen.width - w;
	var y = screen.height - h;
	var debugWindow = window.open('', '', 'width=' + w + ',height=' + h + ',left=' + x + ',top=' + y);

	// switch to window document
	localDoc = debugWindow.document;

	popoutRef.doc = localDoc;
	localDoc.title = 'Debugger - ' + moduleName;
	localDoc.body.style.margin = '0';
	localDoc.body.style.padding = '0';
	var domNode = render(virtualNode, eventNode);
	localDoc.body.appendChild(domNode);

	localDoc.addEventListener('keydown', function(event) {
		if (event.metaKey && event.which === 82)
		{
			window.location.reload();
		}
		if (event.which === 38)
		{
			eventNode.tagger({ ctor: 'Up' });
			event.preventDefault();
		}
		if (event.which === 40)
		{
			eventNode.tagger({ ctor: 'Down' });
			event.preventDefault();
		}
	});

	function close()
	{
		popoutRef.doc = undefined;
		debugWindow.close();
	}
	window.addEventListener('unload', close);
	debugWindow.addEventListener('unload', function() {
		popoutRef.doc = undefined;
		window.removeEventListener('unload', close);
		eventNode.tagger({ ctor: 'Close' });
	});

	// switch back to the normal document
	localDoc = document;

	return domNode;
}


// BLOCK EVENTS

function wrapViewIn(appEventNode, overlayNode, viewIn)
{
	var ignorer = makeIgnorer(overlayNode);
	var blocking = 'Normal';
	var overflow;

	var normalTagger = appEventNode.tagger;
	var blockTagger = function() {};

	return function(model)
	{
		var tuple = viewIn(model);
		var newBlocking = tuple._0.ctor;
		appEventNode.tagger = newBlocking === 'Normal' ? normalTagger : blockTagger;
		if (blocking !== newBlocking)
		{
			traverse('removeEventListener', ignorer, blocking);
			traverse('addEventListener', ignorer, newBlocking);

			if (blocking === 'Normal')
			{
				overflow = document.body.style.overflow;
				document.body.style.overflow = 'hidden';
			}

			if (newBlocking === 'Normal')
			{
				document.body.style.overflow = overflow;
			}

			blocking = newBlocking;
		}
		return tuple._1;
	}
}

function traverse(verbEventListener, ignorer, blocking)
{
	switch(blocking)
	{
		case 'Normal':
			return;

		case 'Pause':
			return traverseHelp(verbEventListener, ignorer, mostEvents);

		case 'Message':
			return traverseHelp(verbEventListener, ignorer, allEvents);
	}
}

function traverseHelp(verbEventListener, handler, eventNames)
{
	for (var i = 0; i < eventNames.length; i++)
	{
		document.body[verbEventListener](eventNames[i], handler, true);
	}
}

function makeIgnorer(overlayNode)
{
	return function(event)
	{
		if (event.type === 'keydown' && event.metaKey && event.which === 82)
		{
			return;
		}

		var isScroll = event.type === 'scroll' || event.type === 'wheel';

		var node = event.target;
		while (node !== null)
		{
			if (node.className === 'elm-overlay-message-details' && isScroll)
			{
				return;
			}

			if (node === overlayNode && !isScroll)
			{
				return;
			}
			node = node.parentNode;
		}

		event.stopPropagation();
		event.preventDefault();
	}
}

var mostEvents = [
	'click', 'dblclick', 'mousemove',
	'mouseup', 'mousedown', 'mouseenter', 'mouseleave',
	'touchstart', 'touchend', 'touchcancel', 'touchmove',
	'pointerdown', 'pointerup', 'pointerover', 'pointerout',
	'pointerenter', 'pointerleave', 'pointermove', 'pointercancel',
	'dragstart', 'drag', 'dragend', 'dragenter', 'dragover', 'dragleave', 'drop',
	'keyup', 'keydown', 'keypress',
	'input', 'change',
	'focus', 'blur'
];

var allEvents = mostEvents.concat('wheel', 'scroll');


return {
	node: node,
	text: text,
	custom: custom,
	map: F2(map),

	on: F3(on),
	style: style,
	property: F2(property),
	attribute: F2(attribute),
	attributeNS: F3(attributeNS),
	mapProperty: F2(mapProperty),

	lazy: F2(lazy),
	lazy2: F3(lazy2),
	lazy3: F4(lazy3),
	keyedNode: F3(keyedNode),

	program: program,
	programWithFlags: programWithFlags,
	staticProgram: staticProgram
};

}();

var _elm_lang$virtual_dom$VirtualDom$programWithFlags = function (impl) {
	return A2(_elm_lang$virtual_dom$Native_VirtualDom.programWithFlags, _elm_lang$virtual_dom$VirtualDom_Debug$wrapWithFlags, impl);
};
var _elm_lang$virtual_dom$VirtualDom$program = function (impl) {
	return A2(_elm_lang$virtual_dom$Native_VirtualDom.program, _elm_lang$virtual_dom$VirtualDom_Debug$wrap, impl);
};
var _elm_lang$virtual_dom$VirtualDom$keyedNode = _elm_lang$virtual_dom$Native_VirtualDom.keyedNode;
var _elm_lang$virtual_dom$VirtualDom$lazy3 = _elm_lang$virtual_dom$Native_VirtualDom.lazy3;
var _elm_lang$virtual_dom$VirtualDom$lazy2 = _elm_lang$virtual_dom$Native_VirtualDom.lazy2;
var _elm_lang$virtual_dom$VirtualDom$lazy = _elm_lang$virtual_dom$Native_VirtualDom.lazy;
var _elm_lang$virtual_dom$VirtualDom$defaultOptions = {stopPropagation: false, preventDefault: false};
var _elm_lang$virtual_dom$VirtualDom$onWithOptions = _elm_lang$virtual_dom$Native_VirtualDom.on;
var _elm_lang$virtual_dom$VirtualDom$on = F2(
	function (eventName, decoder) {
		return A3(_elm_lang$virtual_dom$VirtualDom$onWithOptions, eventName, _elm_lang$virtual_dom$VirtualDom$defaultOptions, decoder);
	});
var _elm_lang$virtual_dom$VirtualDom$style = _elm_lang$virtual_dom$Native_VirtualDom.style;
var _elm_lang$virtual_dom$VirtualDom$mapProperty = _elm_lang$virtual_dom$Native_VirtualDom.mapProperty;
var _elm_lang$virtual_dom$VirtualDom$attributeNS = _elm_lang$virtual_dom$Native_VirtualDom.attributeNS;
var _elm_lang$virtual_dom$VirtualDom$attribute = _elm_lang$virtual_dom$Native_VirtualDom.attribute;
var _elm_lang$virtual_dom$VirtualDom$property = _elm_lang$virtual_dom$Native_VirtualDom.property;
var _elm_lang$virtual_dom$VirtualDom$map = _elm_lang$virtual_dom$Native_VirtualDom.map;
var _elm_lang$virtual_dom$VirtualDom$text = _elm_lang$virtual_dom$Native_VirtualDom.text;
var _elm_lang$virtual_dom$VirtualDom$node = _elm_lang$virtual_dom$Native_VirtualDom.node;
var _elm_lang$virtual_dom$VirtualDom$Options = F2(
	function (a, b) {
		return {stopPropagation: a, preventDefault: b};
	});
var _elm_lang$virtual_dom$VirtualDom$Node = {ctor: 'Node'};
var _elm_lang$virtual_dom$VirtualDom$Property = {ctor: 'Property'};

var _elm_lang$html$Html$programWithFlags = _elm_lang$virtual_dom$VirtualDom$programWithFlags;
var _elm_lang$html$Html$program = _elm_lang$virtual_dom$VirtualDom$program;
var _elm_lang$html$Html$beginnerProgram = function (_p0) {
	var _p1 = _p0;
	return _elm_lang$html$Html$program(
		{
			init: A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_p1.model,
				{ctor: '[]'}),
			update: F2(
				function (msg, model) {
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						A2(_p1.update, msg, model),
						{ctor: '[]'});
				}),
			view: _p1.view,
			subscriptions: function (_p2) {
				return _elm_lang$core$Platform_Sub$none;
			}
		});
};
var _elm_lang$html$Html$map = _elm_lang$virtual_dom$VirtualDom$map;
var _elm_lang$html$Html$text = _elm_lang$virtual_dom$VirtualDom$text;
var _elm_lang$html$Html$node = _elm_lang$virtual_dom$VirtualDom$node;
var _elm_lang$html$Html$body = _elm_lang$html$Html$node('body');
var _elm_lang$html$Html$section = _elm_lang$html$Html$node('section');
var _elm_lang$html$Html$nav = _elm_lang$html$Html$node('nav');
var _elm_lang$html$Html$article = _elm_lang$html$Html$node('article');
var _elm_lang$html$Html$aside = _elm_lang$html$Html$node('aside');
var _elm_lang$html$Html$h1 = _elm_lang$html$Html$node('h1');
var _elm_lang$html$Html$h2 = _elm_lang$html$Html$node('h2');
var _elm_lang$html$Html$h3 = _elm_lang$html$Html$node('h3');
var _elm_lang$html$Html$h4 = _elm_lang$html$Html$node('h4');
var _elm_lang$html$Html$h5 = _elm_lang$html$Html$node('h5');
var _elm_lang$html$Html$h6 = _elm_lang$html$Html$node('h6');
var _elm_lang$html$Html$header = _elm_lang$html$Html$node('header');
var _elm_lang$html$Html$footer = _elm_lang$html$Html$node('footer');
var _elm_lang$html$Html$address = _elm_lang$html$Html$node('address');
var _elm_lang$html$Html$main_ = _elm_lang$html$Html$node('main');
var _elm_lang$html$Html$p = _elm_lang$html$Html$node('p');
var _elm_lang$html$Html$hr = _elm_lang$html$Html$node('hr');
var _elm_lang$html$Html$pre = _elm_lang$html$Html$node('pre');
var _elm_lang$html$Html$blockquote = _elm_lang$html$Html$node('blockquote');
var _elm_lang$html$Html$ol = _elm_lang$html$Html$node('ol');
var _elm_lang$html$Html$ul = _elm_lang$html$Html$node('ul');
var _elm_lang$html$Html$li = _elm_lang$html$Html$node('li');
var _elm_lang$html$Html$dl = _elm_lang$html$Html$node('dl');
var _elm_lang$html$Html$dt = _elm_lang$html$Html$node('dt');
var _elm_lang$html$Html$dd = _elm_lang$html$Html$node('dd');
var _elm_lang$html$Html$figure = _elm_lang$html$Html$node('figure');
var _elm_lang$html$Html$figcaption = _elm_lang$html$Html$node('figcaption');
var _elm_lang$html$Html$div = _elm_lang$html$Html$node('div');
var _elm_lang$html$Html$a = _elm_lang$html$Html$node('a');
var _elm_lang$html$Html$em = _elm_lang$html$Html$node('em');
var _elm_lang$html$Html$strong = _elm_lang$html$Html$node('strong');
var _elm_lang$html$Html$small = _elm_lang$html$Html$node('small');
var _elm_lang$html$Html$s = _elm_lang$html$Html$node('s');
var _elm_lang$html$Html$cite = _elm_lang$html$Html$node('cite');
var _elm_lang$html$Html$q = _elm_lang$html$Html$node('q');
var _elm_lang$html$Html$dfn = _elm_lang$html$Html$node('dfn');
var _elm_lang$html$Html$abbr = _elm_lang$html$Html$node('abbr');
var _elm_lang$html$Html$time = _elm_lang$html$Html$node('time');
var _elm_lang$html$Html$code = _elm_lang$html$Html$node('code');
var _elm_lang$html$Html$var = _elm_lang$html$Html$node('var');
var _elm_lang$html$Html$samp = _elm_lang$html$Html$node('samp');
var _elm_lang$html$Html$kbd = _elm_lang$html$Html$node('kbd');
var _elm_lang$html$Html$sub = _elm_lang$html$Html$node('sub');
var _elm_lang$html$Html$sup = _elm_lang$html$Html$node('sup');
var _elm_lang$html$Html$i = _elm_lang$html$Html$node('i');
var _elm_lang$html$Html$b = _elm_lang$html$Html$node('b');
var _elm_lang$html$Html$u = _elm_lang$html$Html$node('u');
var _elm_lang$html$Html$mark = _elm_lang$html$Html$node('mark');
var _elm_lang$html$Html$ruby = _elm_lang$html$Html$node('ruby');
var _elm_lang$html$Html$rt = _elm_lang$html$Html$node('rt');
var _elm_lang$html$Html$rp = _elm_lang$html$Html$node('rp');
var _elm_lang$html$Html$bdi = _elm_lang$html$Html$node('bdi');
var _elm_lang$html$Html$bdo = _elm_lang$html$Html$node('bdo');
var _elm_lang$html$Html$span = _elm_lang$html$Html$node('span');
var _elm_lang$html$Html$br = _elm_lang$html$Html$node('br');
var _elm_lang$html$Html$wbr = _elm_lang$html$Html$node('wbr');
var _elm_lang$html$Html$ins = _elm_lang$html$Html$node('ins');
var _elm_lang$html$Html$del = _elm_lang$html$Html$node('del');
var _elm_lang$html$Html$img = _elm_lang$html$Html$node('img');
var _elm_lang$html$Html$iframe = _elm_lang$html$Html$node('iframe');
var _elm_lang$html$Html$embed = _elm_lang$html$Html$node('embed');
var _elm_lang$html$Html$object = _elm_lang$html$Html$node('object');
var _elm_lang$html$Html$param = _elm_lang$html$Html$node('param');
var _elm_lang$html$Html$video = _elm_lang$html$Html$node('video');
var _elm_lang$html$Html$audio = _elm_lang$html$Html$node('audio');
var _elm_lang$html$Html$source = _elm_lang$html$Html$node('source');
var _elm_lang$html$Html$track = _elm_lang$html$Html$node('track');
var _elm_lang$html$Html$canvas = _elm_lang$html$Html$node('canvas');
var _elm_lang$html$Html$math = _elm_lang$html$Html$node('math');
var _elm_lang$html$Html$table = _elm_lang$html$Html$node('table');
var _elm_lang$html$Html$caption = _elm_lang$html$Html$node('caption');
var _elm_lang$html$Html$colgroup = _elm_lang$html$Html$node('colgroup');
var _elm_lang$html$Html$col = _elm_lang$html$Html$node('col');
var _elm_lang$html$Html$tbody = _elm_lang$html$Html$node('tbody');
var _elm_lang$html$Html$thead = _elm_lang$html$Html$node('thead');
var _elm_lang$html$Html$tfoot = _elm_lang$html$Html$node('tfoot');
var _elm_lang$html$Html$tr = _elm_lang$html$Html$node('tr');
var _elm_lang$html$Html$td = _elm_lang$html$Html$node('td');
var _elm_lang$html$Html$th = _elm_lang$html$Html$node('th');
var _elm_lang$html$Html$form = _elm_lang$html$Html$node('form');
var _elm_lang$html$Html$fieldset = _elm_lang$html$Html$node('fieldset');
var _elm_lang$html$Html$legend = _elm_lang$html$Html$node('legend');
var _elm_lang$html$Html$label = _elm_lang$html$Html$node('label');
var _elm_lang$html$Html$input = _elm_lang$html$Html$node('input');
var _elm_lang$html$Html$button = _elm_lang$html$Html$node('button');
var _elm_lang$html$Html$select = _elm_lang$html$Html$node('select');
var _elm_lang$html$Html$datalist = _elm_lang$html$Html$node('datalist');
var _elm_lang$html$Html$optgroup = _elm_lang$html$Html$node('optgroup');
var _elm_lang$html$Html$option = _elm_lang$html$Html$node('option');
var _elm_lang$html$Html$textarea = _elm_lang$html$Html$node('textarea');
var _elm_lang$html$Html$keygen = _elm_lang$html$Html$node('keygen');
var _elm_lang$html$Html$output = _elm_lang$html$Html$node('output');
var _elm_lang$html$Html$progress = _elm_lang$html$Html$node('progress');
var _elm_lang$html$Html$meter = _elm_lang$html$Html$node('meter');
var _elm_lang$html$Html$details = _elm_lang$html$Html$node('details');
var _elm_lang$html$Html$summary = _elm_lang$html$Html$node('summary');
var _elm_lang$html$Html$menuitem = _elm_lang$html$Html$node('menuitem');
var _elm_lang$html$Html$menu = _elm_lang$html$Html$node('menu');

var _elm_lang$html$Html_Attributes$map = _elm_lang$virtual_dom$VirtualDom$mapProperty;
var _elm_lang$html$Html_Attributes$attribute = _elm_lang$virtual_dom$VirtualDom$attribute;
var _elm_lang$html$Html_Attributes$contextmenu = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'contextmenu', value);
};
var _elm_lang$html$Html_Attributes$draggable = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'draggable', value);
};
var _elm_lang$html$Html_Attributes$itemprop = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'itemprop', value);
};
var _elm_lang$html$Html_Attributes$tabindex = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'tabIndex',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$charset = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'charset', value);
};
var _elm_lang$html$Html_Attributes$height = function (value) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'height',
		_elm_lang$core$Basics$toString(value));
};
var _elm_lang$html$Html_Attributes$width = function (value) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'width',
		_elm_lang$core$Basics$toString(value));
};
var _elm_lang$html$Html_Attributes$formaction = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'formAction', value);
};
var _elm_lang$html$Html_Attributes$list = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'list', value);
};
var _elm_lang$html$Html_Attributes$minlength = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'minLength',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$maxlength = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'maxlength',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$size = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'size',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$form = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'form', value);
};
var _elm_lang$html$Html_Attributes$cols = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'cols',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$rows = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'rows',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$challenge = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'challenge', value);
};
var _elm_lang$html$Html_Attributes$media = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'media', value);
};
var _elm_lang$html$Html_Attributes$rel = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'rel', value);
};
var _elm_lang$html$Html_Attributes$datetime = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'datetime', value);
};
var _elm_lang$html$Html_Attributes$pubdate = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'pubdate', value);
};
var _elm_lang$html$Html_Attributes$colspan = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'colspan',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$rowspan = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'rowspan',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$manifest = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'manifest', value);
};
var _elm_lang$html$Html_Attributes$property = _elm_lang$virtual_dom$VirtualDom$property;
var _elm_lang$html$Html_Attributes$stringProperty = F2(
	function (name, string) {
		return A2(
			_elm_lang$html$Html_Attributes$property,
			name,
			_elm_lang$core$Json_Encode$string(string));
	});
var _elm_lang$html$Html_Attributes$class = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'className', name);
};
var _elm_lang$html$Html_Attributes$id = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'id', name);
};
var _elm_lang$html$Html_Attributes$title = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'title', name);
};
var _elm_lang$html$Html_Attributes$accesskey = function ($char) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'accessKey',
		_elm_lang$core$String$fromChar($char));
};
var _elm_lang$html$Html_Attributes$dir = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'dir', value);
};
var _elm_lang$html$Html_Attributes$dropzone = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'dropzone', value);
};
var _elm_lang$html$Html_Attributes$lang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'lang', value);
};
var _elm_lang$html$Html_Attributes$content = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'content', value);
};
var _elm_lang$html$Html_Attributes$httpEquiv = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'httpEquiv', value);
};
var _elm_lang$html$Html_Attributes$language = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'language', value);
};
var _elm_lang$html$Html_Attributes$src = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'src', value);
};
var _elm_lang$html$Html_Attributes$alt = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'alt', value);
};
var _elm_lang$html$Html_Attributes$preload = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'preload', value);
};
var _elm_lang$html$Html_Attributes$poster = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'poster', value);
};
var _elm_lang$html$Html_Attributes$kind = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'kind', value);
};
var _elm_lang$html$Html_Attributes$srclang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'srclang', value);
};
var _elm_lang$html$Html_Attributes$sandbox = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'sandbox', value);
};
var _elm_lang$html$Html_Attributes$srcdoc = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'srcdoc', value);
};
var _elm_lang$html$Html_Attributes$type_ = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'type', value);
};
var _elm_lang$html$Html_Attributes$value = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'value', value);
};
var _elm_lang$html$Html_Attributes$defaultValue = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'defaultValue', value);
};
var _elm_lang$html$Html_Attributes$placeholder = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'placeholder', value);
};
var _elm_lang$html$Html_Attributes$accept = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'accept', value);
};
var _elm_lang$html$Html_Attributes$acceptCharset = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'acceptCharset', value);
};
var _elm_lang$html$Html_Attributes$action = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'action', value);
};
var _elm_lang$html$Html_Attributes$autocomplete = function (bool) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'autocomplete',
		bool ? 'on' : 'off');
};
var _elm_lang$html$Html_Attributes$enctype = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'enctype', value);
};
var _elm_lang$html$Html_Attributes$method = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'method', value);
};
var _elm_lang$html$Html_Attributes$name = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'name', value);
};
var _elm_lang$html$Html_Attributes$pattern = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'pattern', value);
};
var _elm_lang$html$Html_Attributes$for = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'htmlFor', value);
};
var _elm_lang$html$Html_Attributes$max = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'max', value);
};
var _elm_lang$html$Html_Attributes$min = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'min', value);
};
var _elm_lang$html$Html_Attributes$step = function (n) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'step', n);
};
var _elm_lang$html$Html_Attributes$wrap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'wrap', value);
};
var _elm_lang$html$Html_Attributes$usemap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'useMap', value);
};
var _elm_lang$html$Html_Attributes$shape = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'shape', value);
};
var _elm_lang$html$Html_Attributes$coords = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'coords', value);
};
var _elm_lang$html$Html_Attributes$keytype = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'keytype', value);
};
var _elm_lang$html$Html_Attributes$align = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'align', value);
};
var _elm_lang$html$Html_Attributes$cite = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'cite', value);
};
var _elm_lang$html$Html_Attributes$href = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'href', value);
};
var _elm_lang$html$Html_Attributes$target = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'target', value);
};
var _elm_lang$html$Html_Attributes$downloadAs = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'download', value);
};
var _elm_lang$html$Html_Attributes$hreflang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'hreflang', value);
};
var _elm_lang$html$Html_Attributes$ping = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'ping', value);
};
var _elm_lang$html$Html_Attributes$start = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'start',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$headers = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'headers', value);
};
var _elm_lang$html$Html_Attributes$scope = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'scope', value);
};
var _elm_lang$html$Html_Attributes$boolProperty = F2(
	function (name, bool) {
		return A2(
			_elm_lang$html$Html_Attributes$property,
			name,
			_elm_lang$core$Json_Encode$bool(bool));
	});
var _elm_lang$html$Html_Attributes$hidden = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'hidden', bool);
};
var _elm_lang$html$Html_Attributes$contenteditable = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'contentEditable', bool);
};
var _elm_lang$html$Html_Attributes$spellcheck = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'spellcheck', bool);
};
var _elm_lang$html$Html_Attributes$async = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'async', bool);
};
var _elm_lang$html$Html_Attributes$defer = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'defer', bool);
};
var _elm_lang$html$Html_Attributes$scoped = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'scoped', bool);
};
var _elm_lang$html$Html_Attributes$autoplay = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'autoplay', bool);
};
var _elm_lang$html$Html_Attributes$controls = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'controls', bool);
};
var _elm_lang$html$Html_Attributes$loop = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'loop', bool);
};
var _elm_lang$html$Html_Attributes$default = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'default', bool);
};
var _elm_lang$html$Html_Attributes$seamless = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'seamless', bool);
};
var _elm_lang$html$Html_Attributes$checked = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'checked', bool);
};
var _elm_lang$html$Html_Attributes$selected = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'selected', bool);
};
var _elm_lang$html$Html_Attributes$autofocus = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'autofocus', bool);
};
var _elm_lang$html$Html_Attributes$disabled = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'disabled', bool);
};
var _elm_lang$html$Html_Attributes$multiple = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'multiple', bool);
};
var _elm_lang$html$Html_Attributes$novalidate = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'noValidate', bool);
};
var _elm_lang$html$Html_Attributes$readonly = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'readOnly', bool);
};
var _elm_lang$html$Html_Attributes$required = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'required', bool);
};
var _elm_lang$html$Html_Attributes$ismap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'isMap', value);
};
var _elm_lang$html$Html_Attributes$download = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'download', bool);
};
var _elm_lang$html$Html_Attributes$reversed = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'reversed', bool);
};
var _elm_lang$html$Html_Attributes$classList = function (list) {
	return _elm_lang$html$Html_Attributes$class(
		A2(
			_elm_lang$core$String$join,
			' ',
			A2(
				_elm_lang$core$List$map,
				_elm_lang$core$Tuple$first,
				A2(_elm_lang$core$List$filter, _elm_lang$core$Tuple$second, list))));
};
var _elm_lang$html$Html_Attributes$style = _elm_lang$virtual_dom$VirtualDom$style;

var _elm_lang$html$Html_Events$keyCode = A2(_elm_lang$core$Json_Decode$field, 'keyCode', _elm_lang$core$Json_Decode$int);
var _elm_lang$html$Html_Events$targetChecked = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'target',
		_1: {
			ctor: '::',
			_0: 'checked',
			_1: {ctor: '[]'}
		}
	},
	_elm_lang$core$Json_Decode$bool);
var _elm_lang$html$Html_Events$targetValue = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'target',
		_1: {
			ctor: '::',
			_0: 'value',
			_1: {ctor: '[]'}
		}
	},
	_elm_lang$core$Json_Decode$string);
var _elm_lang$html$Html_Events$defaultOptions = _elm_lang$virtual_dom$VirtualDom$defaultOptions;
var _elm_lang$html$Html_Events$onWithOptions = _elm_lang$virtual_dom$VirtualDom$onWithOptions;
var _elm_lang$html$Html_Events$on = _elm_lang$virtual_dom$VirtualDom$on;
var _elm_lang$html$Html_Events$onFocus = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'focus',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onBlur = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'blur',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onSubmitOptions = _elm_lang$core$Native_Utils.update(
	_elm_lang$html$Html_Events$defaultOptions,
	{preventDefault: true});
var _elm_lang$html$Html_Events$onSubmit = function (msg) {
	return A3(
		_elm_lang$html$Html_Events$onWithOptions,
		'submit',
		_elm_lang$html$Html_Events$onSubmitOptions,
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onCheck = function (tagger) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'change',
		A2(_elm_lang$core$Json_Decode$map, tagger, _elm_lang$html$Html_Events$targetChecked));
};
var _elm_lang$html$Html_Events$onInput = function (tagger) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'input',
		A2(_elm_lang$core$Json_Decode$map, tagger, _elm_lang$html$Html_Events$targetValue));
};
var _elm_lang$html$Html_Events$onMouseOut = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseout',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseOver = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseover',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseLeave = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseleave',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseEnter = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseenter',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseUp = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseup',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseDown = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mousedown',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onDoubleClick = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'dblclick',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onClick = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'click',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$Options = F2(
	function (a, b) {
		return {stopPropagation: a, preventDefault: b};
	});

var _elm_lang$keyboard$Keyboard$onSelfMsg = F3(
	function (router, _p0, state) {
		var _p1 = _p0;
		var _p2 = A2(_elm_lang$core$Dict$get, _p1.category, state);
		if (_p2.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var send = function (tagger) {
				return A2(
					_elm_lang$core$Platform$sendToApp,
					router,
					tagger(_p1.keyCode));
			};
			return A2(
				_elm_lang$core$Task$andThen,
				function (_p3) {
					return _elm_lang$core$Task$succeed(state);
				},
				_elm_lang$core$Task$sequence(
					A2(_elm_lang$core$List$map, send, _p2._0.taggers)));
		}
	});
var _elm_lang$keyboard$Keyboard_ops = _elm_lang$keyboard$Keyboard_ops || {};
_elm_lang$keyboard$Keyboard_ops['&>'] = F2(
	function (task1, task2) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (_p4) {
				return task2;
			},
			task1);
	});
var _elm_lang$keyboard$Keyboard$init = _elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty);
var _elm_lang$keyboard$Keyboard$categorizeHelpHelp = F2(
	function (value, maybeValues) {
		var _p5 = maybeValues;
		if (_p5.ctor === 'Nothing') {
			return _elm_lang$core$Maybe$Just(
				{
					ctor: '::',
					_0: value,
					_1: {ctor: '[]'}
				});
		} else {
			return _elm_lang$core$Maybe$Just(
				{ctor: '::', _0: value, _1: _p5._0});
		}
	});
var _elm_lang$keyboard$Keyboard$categorizeHelp = F2(
	function (subs, subDict) {
		categorizeHelp:
		while (true) {
			var _p6 = subs;
			if (_p6.ctor === '[]') {
				return subDict;
			} else {
				var _v4 = _p6._1,
					_v5 = A3(
					_elm_lang$core$Dict$update,
					_p6._0._0,
					_elm_lang$keyboard$Keyboard$categorizeHelpHelp(_p6._0._1),
					subDict);
				subs = _v4;
				subDict = _v5;
				continue categorizeHelp;
			}
		}
	});
var _elm_lang$keyboard$Keyboard$categorize = function (subs) {
	return A2(_elm_lang$keyboard$Keyboard$categorizeHelp, subs, _elm_lang$core$Dict$empty);
};
var _elm_lang$keyboard$Keyboard$keyCode = A2(_elm_lang$core$Json_Decode$field, 'keyCode', _elm_lang$core$Json_Decode$int);
var _elm_lang$keyboard$Keyboard$subscription = _elm_lang$core$Native_Platform.leaf('Keyboard');
var _elm_lang$keyboard$Keyboard$Watcher = F2(
	function (a, b) {
		return {taggers: a, pid: b};
	});
var _elm_lang$keyboard$Keyboard$Msg = F2(
	function (a, b) {
		return {category: a, keyCode: b};
	});
var _elm_lang$keyboard$Keyboard$onEffects = F3(
	function (router, newSubs, oldState) {
		var rightStep = F3(
			function (category, taggers, task) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (state) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (pid) {
								return _elm_lang$core$Task$succeed(
									A3(
										_elm_lang$core$Dict$insert,
										category,
										A2(_elm_lang$keyboard$Keyboard$Watcher, taggers, pid),
										state));
							},
							_elm_lang$core$Process$spawn(
								A3(
									_elm_lang$dom$Dom_LowLevel$onDocument,
									category,
									_elm_lang$keyboard$Keyboard$keyCode,
									function (_p7) {
										return A2(
											_elm_lang$core$Platform$sendToSelf,
											router,
											A2(_elm_lang$keyboard$Keyboard$Msg, category, _p7));
									})));
					},
					task);
			});
		var bothStep = F4(
			function (category, _p8, taggers, task) {
				var _p9 = _p8;
				return A2(
					_elm_lang$core$Task$map,
					A2(
						_elm_lang$core$Dict$insert,
						category,
						A2(_elm_lang$keyboard$Keyboard$Watcher, taggers, _p9.pid)),
					task);
			});
		var leftStep = F3(
			function (category, _p10, task) {
				var _p11 = _p10;
				return A2(
					_elm_lang$keyboard$Keyboard_ops['&>'],
					_elm_lang$core$Process$kill(_p11.pid),
					task);
			});
		return A6(
			_elm_lang$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			oldState,
			_elm_lang$keyboard$Keyboard$categorize(newSubs),
			_elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty));
	});
var _elm_lang$keyboard$Keyboard$MySub = F2(
	function (a, b) {
		return {ctor: 'MySub', _0: a, _1: b};
	});
var _elm_lang$keyboard$Keyboard$presses = function (tagger) {
	return _elm_lang$keyboard$Keyboard$subscription(
		A2(_elm_lang$keyboard$Keyboard$MySub, 'keypress', tagger));
};
var _elm_lang$keyboard$Keyboard$downs = function (tagger) {
	return _elm_lang$keyboard$Keyboard$subscription(
		A2(_elm_lang$keyboard$Keyboard$MySub, 'keydown', tagger));
};
var _elm_lang$keyboard$Keyboard$ups = function (tagger) {
	return _elm_lang$keyboard$Keyboard$subscription(
		A2(_elm_lang$keyboard$Keyboard$MySub, 'keyup', tagger));
};
var _elm_lang$keyboard$Keyboard$subMap = F2(
	function (func, _p12) {
		var _p13 = _p12;
		return A2(
			_elm_lang$keyboard$Keyboard$MySub,
			_p13._0,
			function (_p14) {
				return func(
					_p13._1(_p14));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Keyboard'] = {pkg: 'elm-lang/keyboard', init: _elm_lang$keyboard$Keyboard$init, onEffects: _elm_lang$keyboard$Keyboard$onEffects, onSelfMsg: _elm_lang$keyboard$Keyboard$onSelfMsg, tag: 'sub', subMap: _elm_lang$keyboard$Keyboard$subMap};

var _elm_lang$mouse$Mouse_ops = _elm_lang$mouse$Mouse_ops || {};
_elm_lang$mouse$Mouse_ops['&>'] = F2(
	function (t1, t2) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (_p0) {
				return t2;
			},
			t1);
	});
var _elm_lang$mouse$Mouse$onSelfMsg = F3(
	function (router, _p1, state) {
		var _p2 = _p1;
		var _p3 = A2(_elm_lang$core$Dict$get, _p2.category, state);
		if (_p3.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var send = function (tagger) {
				return A2(
					_elm_lang$core$Platform$sendToApp,
					router,
					tagger(_p2.position));
			};
			return A2(
				_elm_lang$mouse$Mouse_ops['&>'],
				_elm_lang$core$Task$sequence(
					A2(_elm_lang$core$List$map, send, _p3._0.taggers)),
				_elm_lang$core$Task$succeed(state));
		}
	});
var _elm_lang$mouse$Mouse$init = _elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty);
var _elm_lang$mouse$Mouse$categorizeHelpHelp = F2(
	function (value, maybeValues) {
		var _p4 = maybeValues;
		if (_p4.ctor === 'Nothing') {
			return _elm_lang$core$Maybe$Just(
				{
					ctor: '::',
					_0: value,
					_1: {ctor: '[]'}
				});
		} else {
			return _elm_lang$core$Maybe$Just(
				{ctor: '::', _0: value, _1: _p4._0});
		}
	});
var _elm_lang$mouse$Mouse$categorizeHelp = F2(
	function (subs, subDict) {
		categorizeHelp:
		while (true) {
			var _p5 = subs;
			if (_p5.ctor === '[]') {
				return subDict;
			} else {
				var _v4 = _p5._1,
					_v5 = A3(
					_elm_lang$core$Dict$update,
					_p5._0._0,
					_elm_lang$mouse$Mouse$categorizeHelpHelp(_p5._0._1),
					subDict);
				subs = _v4;
				subDict = _v5;
				continue categorizeHelp;
			}
		}
	});
var _elm_lang$mouse$Mouse$categorize = function (subs) {
	return A2(_elm_lang$mouse$Mouse$categorizeHelp, subs, _elm_lang$core$Dict$empty);
};
var _elm_lang$mouse$Mouse$subscription = _elm_lang$core$Native_Platform.leaf('Mouse');
var _elm_lang$mouse$Mouse$Position = F2(
	function (a, b) {
		return {x: a, y: b};
	});
var _elm_lang$mouse$Mouse$position = A3(
	_elm_lang$core$Json_Decode$map2,
	_elm_lang$mouse$Mouse$Position,
	A2(_elm_lang$core$Json_Decode$field, 'pageX', _elm_lang$core$Json_Decode$int),
	A2(_elm_lang$core$Json_Decode$field, 'pageY', _elm_lang$core$Json_Decode$int));
var _elm_lang$mouse$Mouse$Watcher = F2(
	function (a, b) {
		return {taggers: a, pid: b};
	});
var _elm_lang$mouse$Mouse$Msg = F2(
	function (a, b) {
		return {category: a, position: b};
	});
var _elm_lang$mouse$Mouse$onEffects = F3(
	function (router, newSubs, oldState) {
		var rightStep = F3(
			function (category, taggers, task) {
				var tracker = A3(
					_elm_lang$dom$Dom_LowLevel$onDocument,
					category,
					_elm_lang$mouse$Mouse$position,
					function (_p6) {
						return A2(
							_elm_lang$core$Platform$sendToSelf,
							router,
							A2(_elm_lang$mouse$Mouse$Msg, category, _p6));
					});
				return A2(
					_elm_lang$core$Task$andThen,
					function (state) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (pid) {
								return _elm_lang$core$Task$succeed(
									A3(
										_elm_lang$core$Dict$insert,
										category,
										A2(_elm_lang$mouse$Mouse$Watcher, taggers, pid),
										state));
							},
							_elm_lang$core$Process$spawn(tracker));
					},
					task);
			});
		var bothStep = F4(
			function (category, _p7, taggers, task) {
				var _p8 = _p7;
				return A2(
					_elm_lang$core$Task$andThen,
					function (state) {
						return _elm_lang$core$Task$succeed(
							A3(
								_elm_lang$core$Dict$insert,
								category,
								A2(_elm_lang$mouse$Mouse$Watcher, taggers, _p8.pid),
								state));
					},
					task);
			});
		var leftStep = F3(
			function (category, _p9, task) {
				var _p10 = _p9;
				return A2(
					_elm_lang$mouse$Mouse_ops['&>'],
					_elm_lang$core$Process$kill(_p10.pid),
					task);
			});
		return A6(
			_elm_lang$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			oldState,
			_elm_lang$mouse$Mouse$categorize(newSubs),
			_elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty));
	});
var _elm_lang$mouse$Mouse$MySub = F2(
	function (a, b) {
		return {ctor: 'MySub', _0: a, _1: b};
	});
var _elm_lang$mouse$Mouse$clicks = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'click', tagger));
};
var _elm_lang$mouse$Mouse$moves = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'mousemove', tagger));
};
var _elm_lang$mouse$Mouse$downs = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'mousedown', tagger));
};
var _elm_lang$mouse$Mouse$ups = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'mouseup', tagger));
};
var _elm_lang$mouse$Mouse$subMap = F2(
	function (func, _p11) {
		var _p12 = _p11;
		return A2(
			_elm_lang$mouse$Mouse$MySub,
			_p12._0,
			function (_p13) {
				return func(
					_p12._1(_p13));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Mouse'] = {pkg: 'elm-lang/mouse', init: _elm_lang$mouse$Mouse$init, onEffects: _elm_lang$mouse$Mouse$onEffects, onSelfMsg: _elm_lang$mouse$Mouse$onSelfMsg, tag: 'sub', subMap: _elm_lang$mouse$Mouse$subMap};

var _elm_lang$svg$Svg$map = _elm_lang$virtual_dom$VirtualDom$map;
var _elm_lang$svg$Svg$text = _elm_lang$virtual_dom$VirtualDom$text;
var _elm_lang$svg$Svg$svgNamespace = A2(
	_elm_lang$virtual_dom$VirtualDom$property,
	'namespace',
	_elm_lang$core$Json_Encode$string('http://www.w3.org/2000/svg'));
var _elm_lang$svg$Svg$node = F3(
	function (name, attributes, children) {
		return A3(
			_elm_lang$virtual_dom$VirtualDom$node,
			name,
			{ctor: '::', _0: _elm_lang$svg$Svg$svgNamespace, _1: attributes},
			children);
	});
var _elm_lang$svg$Svg$svg = _elm_lang$svg$Svg$node('svg');
var _elm_lang$svg$Svg$foreignObject = _elm_lang$svg$Svg$node('foreignObject');
var _elm_lang$svg$Svg$animate = _elm_lang$svg$Svg$node('animate');
var _elm_lang$svg$Svg$animateColor = _elm_lang$svg$Svg$node('animateColor');
var _elm_lang$svg$Svg$animateMotion = _elm_lang$svg$Svg$node('animateMotion');
var _elm_lang$svg$Svg$animateTransform = _elm_lang$svg$Svg$node('animateTransform');
var _elm_lang$svg$Svg$mpath = _elm_lang$svg$Svg$node('mpath');
var _elm_lang$svg$Svg$set = _elm_lang$svg$Svg$node('set');
var _elm_lang$svg$Svg$a = _elm_lang$svg$Svg$node('a');
var _elm_lang$svg$Svg$defs = _elm_lang$svg$Svg$node('defs');
var _elm_lang$svg$Svg$g = _elm_lang$svg$Svg$node('g');
var _elm_lang$svg$Svg$marker = _elm_lang$svg$Svg$node('marker');
var _elm_lang$svg$Svg$mask = _elm_lang$svg$Svg$node('mask');
var _elm_lang$svg$Svg$pattern = _elm_lang$svg$Svg$node('pattern');
var _elm_lang$svg$Svg$switch = _elm_lang$svg$Svg$node('switch');
var _elm_lang$svg$Svg$symbol = _elm_lang$svg$Svg$node('symbol');
var _elm_lang$svg$Svg$desc = _elm_lang$svg$Svg$node('desc');
var _elm_lang$svg$Svg$metadata = _elm_lang$svg$Svg$node('metadata');
var _elm_lang$svg$Svg$title = _elm_lang$svg$Svg$node('title');
var _elm_lang$svg$Svg$feBlend = _elm_lang$svg$Svg$node('feBlend');
var _elm_lang$svg$Svg$feColorMatrix = _elm_lang$svg$Svg$node('feColorMatrix');
var _elm_lang$svg$Svg$feComponentTransfer = _elm_lang$svg$Svg$node('feComponentTransfer');
var _elm_lang$svg$Svg$feComposite = _elm_lang$svg$Svg$node('feComposite');
var _elm_lang$svg$Svg$feConvolveMatrix = _elm_lang$svg$Svg$node('feConvolveMatrix');
var _elm_lang$svg$Svg$feDiffuseLighting = _elm_lang$svg$Svg$node('feDiffuseLighting');
var _elm_lang$svg$Svg$feDisplacementMap = _elm_lang$svg$Svg$node('feDisplacementMap');
var _elm_lang$svg$Svg$feFlood = _elm_lang$svg$Svg$node('feFlood');
var _elm_lang$svg$Svg$feFuncA = _elm_lang$svg$Svg$node('feFuncA');
var _elm_lang$svg$Svg$feFuncB = _elm_lang$svg$Svg$node('feFuncB');
var _elm_lang$svg$Svg$feFuncG = _elm_lang$svg$Svg$node('feFuncG');
var _elm_lang$svg$Svg$feFuncR = _elm_lang$svg$Svg$node('feFuncR');
var _elm_lang$svg$Svg$feGaussianBlur = _elm_lang$svg$Svg$node('feGaussianBlur');
var _elm_lang$svg$Svg$feImage = _elm_lang$svg$Svg$node('feImage');
var _elm_lang$svg$Svg$feMerge = _elm_lang$svg$Svg$node('feMerge');
var _elm_lang$svg$Svg$feMergeNode = _elm_lang$svg$Svg$node('feMergeNode');
var _elm_lang$svg$Svg$feMorphology = _elm_lang$svg$Svg$node('feMorphology');
var _elm_lang$svg$Svg$feOffset = _elm_lang$svg$Svg$node('feOffset');
var _elm_lang$svg$Svg$feSpecularLighting = _elm_lang$svg$Svg$node('feSpecularLighting');
var _elm_lang$svg$Svg$feTile = _elm_lang$svg$Svg$node('feTile');
var _elm_lang$svg$Svg$feTurbulence = _elm_lang$svg$Svg$node('feTurbulence');
var _elm_lang$svg$Svg$font = _elm_lang$svg$Svg$node('font');
var _elm_lang$svg$Svg$linearGradient = _elm_lang$svg$Svg$node('linearGradient');
var _elm_lang$svg$Svg$radialGradient = _elm_lang$svg$Svg$node('radialGradient');
var _elm_lang$svg$Svg$stop = _elm_lang$svg$Svg$node('stop');
var _elm_lang$svg$Svg$circle = _elm_lang$svg$Svg$node('circle');
var _elm_lang$svg$Svg$ellipse = _elm_lang$svg$Svg$node('ellipse');
var _elm_lang$svg$Svg$image = _elm_lang$svg$Svg$node('image');
var _elm_lang$svg$Svg$line = _elm_lang$svg$Svg$node('line');
var _elm_lang$svg$Svg$path = _elm_lang$svg$Svg$node('path');
var _elm_lang$svg$Svg$polygon = _elm_lang$svg$Svg$node('polygon');
var _elm_lang$svg$Svg$polyline = _elm_lang$svg$Svg$node('polyline');
var _elm_lang$svg$Svg$rect = _elm_lang$svg$Svg$node('rect');
var _elm_lang$svg$Svg$use = _elm_lang$svg$Svg$node('use');
var _elm_lang$svg$Svg$feDistantLight = _elm_lang$svg$Svg$node('feDistantLight');
var _elm_lang$svg$Svg$fePointLight = _elm_lang$svg$Svg$node('fePointLight');
var _elm_lang$svg$Svg$feSpotLight = _elm_lang$svg$Svg$node('feSpotLight');
var _elm_lang$svg$Svg$altGlyph = _elm_lang$svg$Svg$node('altGlyph');
var _elm_lang$svg$Svg$altGlyphDef = _elm_lang$svg$Svg$node('altGlyphDef');
var _elm_lang$svg$Svg$altGlyphItem = _elm_lang$svg$Svg$node('altGlyphItem');
var _elm_lang$svg$Svg$glyph = _elm_lang$svg$Svg$node('glyph');
var _elm_lang$svg$Svg$glyphRef = _elm_lang$svg$Svg$node('glyphRef');
var _elm_lang$svg$Svg$textPath = _elm_lang$svg$Svg$node('textPath');
var _elm_lang$svg$Svg$text_ = _elm_lang$svg$Svg$node('text');
var _elm_lang$svg$Svg$tref = _elm_lang$svg$Svg$node('tref');
var _elm_lang$svg$Svg$tspan = _elm_lang$svg$Svg$node('tspan');
var _elm_lang$svg$Svg$clipPath = _elm_lang$svg$Svg$node('clipPath');
var _elm_lang$svg$Svg$colorProfile = _elm_lang$svg$Svg$node('colorProfile');
var _elm_lang$svg$Svg$cursor = _elm_lang$svg$Svg$node('cursor');
var _elm_lang$svg$Svg$filter = _elm_lang$svg$Svg$node('filter');
var _elm_lang$svg$Svg$script = _elm_lang$svg$Svg$node('script');
var _elm_lang$svg$Svg$style = _elm_lang$svg$Svg$node('style');
var _elm_lang$svg$Svg$view = _elm_lang$svg$Svg$node('view');

var _elm_lang$svg$Svg_Attributes$writingMode = _elm_lang$virtual_dom$VirtualDom$attribute('writing-mode');
var _elm_lang$svg$Svg_Attributes$wordSpacing = _elm_lang$virtual_dom$VirtualDom$attribute('word-spacing');
var _elm_lang$svg$Svg_Attributes$visibility = _elm_lang$virtual_dom$VirtualDom$attribute('visibility');
var _elm_lang$svg$Svg_Attributes$unicodeBidi = _elm_lang$virtual_dom$VirtualDom$attribute('unicode-bidi');
var _elm_lang$svg$Svg_Attributes$textRendering = _elm_lang$virtual_dom$VirtualDom$attribute('text-rendering');
var _elm_lang$svg$Svg_Attributes$textDecoration = _elm_lang$virtual_dom$VirtualDom$attribute('text-decoration');
var _elm_lang$svg$Svg_Attributes$textAnchor = _elm_lang$virtual_dom$VirtualDom$attribute('text-anchor');
var _elm_lang$svg$Svg_Attributes$stroke = _elm_lang$virtual_dom$VirtualDom$attribute('stroke');
var _elm_lang$svg$Svg_Attributes$strokeWidth = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-width');
var _elm_lang$svg$Svg_Attributes$strokeOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-opacity');
var _elm_lang$svg$Svg_Attributes$strokeMiterlimit = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-miterlimit');
var _elm_lang$svg$Svg_Attributes$strokeLinejoin = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-linejoin');
var _elm_lang$svg$Svg_Attributes$strokeLinecap = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-linecap');
var _elm_lang$svg$Svg_Attributes$strokeDashoffset = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-dashoffset');
var _elm_lang$svg$Svg_Attributes$strokeDasharray = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-dasharray');
var _elm_lang$svg$Svg_Attributes$stopOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('stop-opacity');
var _elm_lang$svg$Svg_Attributes$stopColor = _elm_lang$virtual_dom$VirtualDom$attribute('stop-color');
var _elm_lang$svg$Svg_Attributes$shapeRendering = _elm_lang$virtual_dom$VirtualDom$attribute('shape-rendering');
var _elm_lang$svg$Svg_Attributes$pointerEvents = _elm_lang$virtual_dom$VirtualDom$attribute('pointer-events');
var _elm_lang$svg$Svg_Attributes$overflow = _elm_lang$virtual_dom$VirtualDom$attribute('overflow');
var _elm_lang$svg$Svg_Attributes$opacity = _elm_lang$virtual_dom$VirtualDom$attribute('opacity');
var _elm_lang$svg$Svg_Attributes$mask = _elm_lang$virtual_dom$VirtualDom$attribute('mask');
var _elm_lang$svg$Svg_Attributes$markerStart = _elm_lang$virtual_dom$VirtualDom$attribute('marker-start');
var _elm_lang$svg$Svg_Attributes$markerMid = _elm_lang$virtual_dom$VirtualDom$attribute('marker-mid');
var _elm_lang$svg$Svg_Attributes$markerEnd = _elm_lang$virtual_dom$VirtualDom$attribute('marker-end');
var _elm_lang$svg$Svg_Attributes$lightingColor = _elm_lang$virtual_dom$VirtualDom$attribute('lighting-color');
var _elm_lang$svg$Svg_Attributes$letterSpacing = _elm_lang$virtual_dom$VirtualDom$attribute('letter-spacing');
var _elm_lang$svg$Svg_Attributes$kerning = _elm_lang$virtual_dom$VirtualDom$attribute('kerning');
var _elm_lang$svg$Svg_Attributes$imageRendering = _elm_lang$virtual_dom$VirtualDom$attribute('image-rendering');
var _elm_lang$svg$Svg_Attributes$glyphOrientationVertical = _elm_lang$virtual_dom$VirtualDom$attribute('glyph-orientation-vertical');
var _elm_lang$svg$Svg_Attributes$glyphOrientationHorizontal = _elm_lang$virtual_dom$VirtualDom$attribute('glyph-orientation-horizontal');
var _elm_lang$svg$Svg_Attributes$fontWeight = _elm_lang$virtual_dom$VirtualDom$attribute('font-weight');
var _elm_lang$svg$Svg_Attributes$fontVariant = _elm_lang$virtual_dom$VirtualDom$attribute('font-variant');
var _elm_lang$svg$Svg_Attributes$fontStyle = _elm_lang$virtual_dom$VirtualDom$attribute('font-style');
var _elm_lang$svg$Svg_Attributes$fontStretch = _elm_lang$virtual_dom$VirtualDom$attribute('font-stretch');
var _elm_lang$svg$Svg_Attributes$fontSize = _elm_lang$virtual_dom$VirtualDom$attribute('font-size');
var _elm_lang$svg$Svg_Attributes$fontSizeAdjust = _elm_lang$virtual_dom$VirtualDom$attribute('font-size-adjust');
var _elm_lang$svg$Svg_Attributes$fontFamily = _elm_lang$virtual_dom$VirtualDom$attribute('font-family');
var _elm_lang$svg$Svg_Attributes$floodOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('flood-opacity');
var _elm_lang$svg$Svg_Attributes$floodColor = _elm_lang$virtual_dom$VirtualDom$attribute('flood-color');
var _elm_lang$svg$Svg_Attributes$filter = _elm_lang$virtual_dom$VirtualDom$attribute('filter');
var _elm_lang$svg$Svg_Attributes$fill = _elm_lang$virtual_dom$VirtualDom$attribute('fill');
var _elm_lang$svg$Svg_Attributes$fillRule = _elm_lang$virtual_dom$VirtualDom$attribute('fill-rule');
var _elm_lang$svg$Svg_Attributes$fillOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('fill-opacity');
var _elm_lang$svg$Svg_Attributes$enableBackground = _elm_lang$virtual_dom$VirtualDom$attribute('enable-background');
var _elm_lang$svg$Svg_Attributes$dominantBaseline = _elm_lang$virtual_dom$VirtualDom$attribute('dominant-baseline');
var _elm_lang$svg$Svg_Attributes$display = _elm_lang$virtual_dom$VirtualDom$attribute('display');
var _elm_lang$svg$Svg_Attributes$direction = _elm_lang$virtual_dom$VirtualDom$attribute('direction');
var _elm_lang$svg$Svg_Attributes$cursor = _elm_lang$virtual_dom$VirtualDom$attribute('cursor');
var _elm_lang$svg$Svg_Attributes$color = _elm_lang$virtual_dom$VirtualDom$attribute('color');
var _elm_lang$svg$Svg_Attributes$colorRendering = _elm_lang$virtual_dom$VirtualDom$attribute('color-rendering');
var _elm_lang$svg$Svg_Attributes$colorProfile = _elm_lang$virtual_dom$VirtualDom$attribute('color-profile');
var _elm_lang$svg$Svg_Attributes$colorInterpolation = _elm_lang$virtual_dom$VirtualDom$attribute('color-interpolation');
var _elm_lang$svg$Svg_Attributes$colorInterpolationFilters = _elm_lang$virtual_dom$VirtualDom$attribute('color-interpolation-filters');
var _elm_lang$svg$Svg_Attributes$clip = _elm_lang$virtual_dom$VirtualDom$attribute('clip');
var _elm_lang$svg$Svg_Attributes$clipRule = _elm_lang$virtual_dom$VirtualDom$attribute('clip-rule');
var _elm_lang$svg$Svg_Attributes$clipPath = _elm_lang$virtual_dom$VirtualDom$attribute('clip-path');
var _elm_lang$svg$Svg_Attributes$baselineShift = _elm_lang$virtual_dom$VirtualDom$attribute('baseline-shift');
var _elm_lang$svg$Svg_Attributes$alignmentBaseline = _elm_lang$virtual_dom$VirtualDom$attribute('alignment-baseline');
var _elm_lang$svg$Svg_Attributes$zoomAndPan = _elm_lang$virtual_dom$VirtualDom$attribute('zoomAndPan');
var _elm_lang$svg$Svg_Attributes$z = _elm_lang$virtual_dom$VirtualDom$attribute('z');
var _elm_lang$svg$Svg_Attributes$yChannelSelector = _elm_lang$virtual_dom$VirtualDom$attribute('yChannelSelector');
var _elm_lang$svg$Svg_Attributes$y2 = _elm_lang$virtual_dom$VirtualDom$attribute('y2');
var _elm_lang$svg$Svg_Attributes$y1 = _elm_lang$virtual_dom$VirtualDom$attribute('y1');
var _elm_lang$svg$Svg_Attributes$y = _elm_lang$virtual_dom$VirtualDom$attribute('y');
var _elm_lang$svg$Svg_Attributes$xmlSpace = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/XML/1998/namespace', 'xml:space');
var _elm_lang$svg$Svg_Attributes$xmlLang = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/XML/1998/namespace', 'xml:lang');
var _elm_lang$svg$Svg_Attributes$xmlBase = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/XML/1998/namespace', 'xml:base');
var _elm_lang$svg$Svg_Attributes$xlinkType = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:type');
var _elm_lang$svg$Svg_Attributes$xlinkTitle = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:title');
var _elm_lang$svg$Svg_Attributes$xlinkShow = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:show');
var _elm_lang$svg$Svg_Attributes$xlinkRole = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:role');
var _elm_lang$svg$Svg_Attributes$xlinkHref = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:href');
var _elm_lang$svg$Svg_Attributes$xlinkArcrole = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:arcrole');
var _elm_lang$svg$Svg_Attributes$xlinkActuate = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:actuate');
var _elm_lang$svg$Svg_Attributes$xChannelSelector = _elm_lang$virtual_dom$VirtualDom$attribute('xChannelSelector');
var _elm_lang$svg$Svg_Attributes$x2 = _elm_lang$virtual_dom$VirtualDom$attribute('x2');
var _elm_lang$svg$Svg_Attributes$x1 = _elm_lang$virtual_dom$VirtualDom$attribute('x1');
var _elm_lang$svg$Svg_Attributes$xHeight = _elm_lang$virtual_dom$VirtualDom$attribute('x-height');
var _elm_lang$svg$Svg_Attributes$x = _elm_lang$virtual_dom$VirtualDom$attribute('x');
var _elm_lang$svg$Svg_Attributes$widths = _elm_lang$virtual_dom$VirtualDom$attribute('widths');
var _elm_lang$svg$Svg_Attributes$width = _elm_lang$virtual_dom$VirtualDom$attribute('width');
var _elm_lang$svg$Svg_Attributes$viewTarget = _elm_lang$virtual_dom$VirtualDom$attribute('viewTarget');
var _elm_lang$svg$Svg_Attributes$viewBox = _elm_lang$virtual_dom$VirtualDom$attribute('viewBox');
var _elm_lang$svg$Svg_Attributes$vertOriginY = _elm_lang$virtual_dom$VirtualDom$attribute('vert-origin-y');
var _elm_lang$svg$Svg_Attributes$vertOriginX = _elm_lang$virtual_dom$VirtualDom$attribute('vert-origin-x');
var _elm_lang$svg$Svg_Attributes$vertAdvY = _elm_lang$virtual_dom$VirtualDom$attribute('vert-adv-y');
var _elm_lang$svg$Svg_Attributes$version = _elm_lang$virtual_dom$VirtualDom$attribute('version');
var _elm_lang$svg$Svg_Attributes$values = _elm_lang$virtual_dom$VirtualDom$attribute('values');
var _elm_lang$svg$Svg_Attributes$vMathematical = _elm_lang$virtual_dom$VirtualDom$attribute('v-mathematical');
var _elm_lang$svg$Svg_Attributes$vIdeographic = _elm_lang$virtual_dom$VirtualDom$attribute('v-ideographic');
var _elm_lang$svg$Svg_Attributes$vHanging = _elm_lang$virtual_dom$VirtualDom$attribute('v-hanging');
var _elm_lang$svg$Svg_Attributes$vAlphabetic = _elm_lang$virtual_dom$VirtualDom$attribute('v-alphabetic');
var _elm_lang$svg$Svg_Attributes$unitsPerEm = _elm_lang$virtual_dom$VirtualDom$attribute('units-per-em');
var _elm_lang$svg$Svg_Attributes$unicodeRange = _elm_lang$virtual_dom$VirtualDom$attribute('unicode-range');
var _elm_lang$svg$Svg_Attributes$unicode = _elm_lang$virtual_dom$VirtualDom$attribute('unicode');
var _elm_lang$svg$Svg_Attributes$underlineThickness = _elm_lang$virtual_dom$VirtualDom$attribute('underline-thickness');
var _elm_lang$svg$Svg_Attributes$underlinePosition = _elm_lang$virtual_dom$VirtualDom$attribute('underline-position');
var _elm_lang$svg$Svg_Attributes$u2 = _elm_lang$virtual_dom$VirtualDom$attribute('u2');
var _elm_lang$svg$Svg_Attributes$u1 = _elm_lang$virtual_dom$VirtualDom$attribute('u1');
var _elm_lang$svg$Svg_Attributes$type_ = _elm_lang$virtual_dom$VirtualDom$attribute('type');
var _elm_lang$svg$Svg_Attributes$transform = _elm_lang$virtual_dom$VirtualDom$attribute('transform');
var _elm_lang$svg$Svg_Attributes$to = _elm_lang$virtual_dom$VirtualDom$attribute('to');
var _elm_lang$svg$Svg_Attributes$title = _elm_lang$virtual_dom$VirtualDom$attribute('title');
var _elm_lang$svg$Svg_Attributes$textLength = _elm_lang$virtual_dom$VirtualDom$attribute('textLength');
var _elm_lang$svg$Svg_Attributes$targetY = _elm_lang$virtual_dom$VirtualDom$attribute('targetY');
var _elm_lang$svg$Svg_Attributes$targetX = _elm_lang$virtual_dom$VirtualDom$attribute('targetX');
var _elm_lang$svg$Svg_Attributes$target = _elm_lang$virtual_dom$VirtualDom$attribute('target');
var _elm_lang$svg$Svg_Attributes$tableValues = _elm_lang$virtual_dom$VirtualDom$attribute('tableValues');
var _elm_lang$svg$Svg_Attributes$systemLanguage = _elm_lang$virtual_dom$VirtualDom$attribute('systemLanguage');
var _elm_lang$svg$Svg_Attributes$surfaceScale = _elm_lang$virtual_dom$VirtualDom$attribute('surfaceScale');
var _elm_lang$svg$Svg_Attributes$style = _elm_lang$virtual_dom$VirtualDom$attribute('style');
var _elm_lang$svg$Svg_Attributes$string = _elm_lang$virtual_dom$VirtualDom$attribute('string');
var _elm_lang$svg$Svg_Attributes$strikethroughThickness = _elm_lang$virtual_dom$VirtualDom$attribute('strikethrough-thickness');
var _elm_lang$svg$Svg_Attributes$strikethroughPosition = _elm_lang$virtual_dom$VirtualDom$attribute('strikethrough-position');
var _elm_lang$svg$Svg_Attributes$stitchTiles = _elm_lang$virtual_dom$VirtualDom$attribute('stitchTiles');
var _elm_lang$svg$Svg_Attributes$stemv = _elm_lang$virtual_dom$VirtualDom$attribute('stemv');
var _elm_lang$svg$Svg_Attributes$stemh = _elm_lang$virtual_dom$VirtualDom$attribute('stemh');
var _elm_lang$svg$Svg_Attributes$stdDeviation = _elm_lang$virtual_dom$VirtualDom$attribute('stdDeviation');
var _elm_lang$svg$Svg_Attributes$startOffset = _elm_lang$virtual_dom$VirtualDom$attribute('startOffset');
var _elm_lang$svg$Svg_Attributes$spreadMethod = _elm_lang$virtual_dom$VirtualDom$attribute('spreadMethod');
var _elm_lang$svg$Svg_Attributes$speed = _elm_lang$virtual_dom$VirtualDom$attribute('speed');
var _elm_lang$svg$Svg_Attributes$specularExponent = _elm_lang$virtual_dom$VirtualDom$attribute('specularExponent');
var _elm_lang$svg$Svg_Attributes$specularConstant = _elm_lang$virtual_dom$VirtualDom$attribute('specularConstant');
var _elm_lang$svg$Svg_Attributes$spacing = _elm_lang$virtual_dom$VirtualDom$attribute('spacing');
var _elm_lang$svg$Svg_Attributes$slope = _elm_lang$virtual_dom$VirtualDom$attribute('slope');
var _elm_lang$svg$Svg_Attributes$seed = _elm_lang$virtual_dom$VirtualDom$attribute('seed');
var _elm_lang$svg$Svg_Attributes$scale = _elm_lang$virtual_dom$VirtualDom$attribute('scale');
var _elm_lang$svg$Svg_Attributes$ry = _elm_lang$virtual_dom$VirtualDom$attribute('ry');
var _elm_lang$svg$Svg_Attributes$rx = _elm_lang$virtual_dom$VirtualDom$attribute('rx');
var _elm_lang$svg$Svg_Attributes$rotate = _elm_lang$virtual_dom$VirtualDom$attribute('rotate');
var _elm_lang$svg$Svg_Attributes$result = _elm_lang$virtual_dom$VirtualDom$attribute('result');
var _elm_lang$svg$Svg_Attributes$restart = _elm_lang$virtual_dom$VirtualDom$attribute('restart');
var _elm_lang$svg$Svg_Attributes$requiredFeatures = _elm_lang$virtual_dom$VirtualDom$attribute('requiredFeatures');
var _elm_lang$svg$Svg_Attributes$requiredExtensions = _elm_lang$virtual_dom$VirtualDom$attribute('requiredExtensions');
var _elm_lang$svg$Svg_Attributes$repeatDur = _elm_lang$virtual_dom$VirtualDom$attribute('repeatDur');
var _elm_lang$svg$Svg_Attributes$repeatCount = _elm_lang$virtual_dom$VirtualDom$attribute('repeatCount');
var _elm_lang$svg$Svg_Attributes$renderingIntent = _elm_lang$virtual_dom$VirtualDom$attribute('rendering-intent');
var _elm_lang$svg$Svg_Attributes$refY = _elm_lang$virtual_dom$VirtualDom$attribute('refY');
var _elm_lang$svg$Svg_Attributes$refX = _elm_lang$virtual_dom$VirtualDom$attribute('refX');
var _elm_lang$svg$Svg_Attributes$radius = _elm_lang$virtual_dom$VirtualDom$attribute('radius');
var _elm_lang$svg$Svg_Attributes$r = _elm_lang$virtual_dom$VirtualDom$attribute('r');
var _elm_lang$svg$Svg_Attributes$primitiveUnits = _elm_lang$virtual_dom$VirtualDom$attribute('primitiveUnits');
var _elm_lang$svg$Svg_Attributes$preserveAspectRatio = _elm_lang$virtual_dom$VirtualDom$attribute('preserveAspectRatio');
var _elm_lang$svg$Svg_Attributes$preserveAlpha = _elm_lang$virtual_dom$VirtualDom$attribute('preserveAlpha');
var _elm_lang$svg$Svg_Attributes$pointsAtZ = _elm_lang$virtual_dom$VirtualDom$attribute('pointsAtZ');
var _elm_lang$svg$Svg_Attributes$pointsAtY = _elm_lang$virtual_dom$VirtualDom$attribute('pointsAtY');
var _elm_lang$svg$Svg_Attributes$pointsAtX = _elm_lang$virtual_dom$VirtualDom$attribute('pointsAtX');
var _elm_lang$svg$Svg_Attributes$points = _elm_lang$virtual_dom$VirtualDom$attribute('points');
var _elm_lang$svg$Svg_Attributes$pointOrder = _elm_lang$virtual_dom$VirtualDom$attribute('point-order');
var _elm_lang$svg$Svg_Attributes$patternUnits = _elm_lang$virtual_dom$VirtualDom$attribute('patternUnits');
var _elm_lang$svg$Svg_Attributes$patternTransform = _elm_lang$virtual_dom$VirtualDom$attribute('patternTransform');
var _elm_lang$svg$Svg_Attributes$patternContentUnits = _elm_lang$virtual_dom$VirtualDom$attribute('patternContentUnits');
var _elm_lang$svg$Svg_Attributes$pathLength = _elm_lang$virtual_dom$VirtualDom$attribute('pathLength');
var _elm_lang$svg$Svg_Attributes$path = _elm_lang$virtual_dom$VirtualDom$attribute('path');
var _elm_lang$svg$Svg_Attributes$panose1 = _elm_lang$virtual_dom$VirtualDom$attribute('panose-1');
var _elm_lang$svg$Svg_Attributes$overlineThickness = _elm_lang$virtual_dom$VirtualDom$attribute('overline-thickness');
var _elm_lang$svg$Svg_Attributes$overlinePosition = _elm_lang$virtual_dom$VirtualDom$attribute('overline-position');
var _elm_lang$svg$Svg_Attributes$origin = _elm_lang$virtual_dom$VirtualDom$attribute('origin');
var _elm_lang$svg$Svg_Attributes$orientation = _elm_lang$virtual_dom$VirtualDom$attribute('orientation');
var _elm_lang$svg$Svg_Attributes$orient = _elm_lang$virtual_dom$VirtualDom$attribute('orient');
var _elm_lang$svg$Svg_Attributes$order = _elm_lang$virtual_dom$VirtualDom$attribute('order');
var _elm_lang$svg$Svg_Attributes$operator = _elm_lang$virtual_dom$VirtualDom$attribute('operator');
var _elm_lang$svg$Svg_Attributes$offset = _elm_lang$virtual_dom$VirtualDom$attribute('offset');
var _elm_lang$svg$Svg_Attributes$numOctaves = _elm_lang$virtual_dom$VirtualDom$attribute('numOctaves');
var _elm_lang$svg$Svg_Attributes$name = _elm_lang$virtual_dom$VirtualDom$attribute('name');
var _elm_lang$svg$Svg_Attributes$mode = _elm_lang$virtual_dom$VirtualDom$attribute('mode');
var _elm_lang$svg$Svg_Attributes$min = _elm_lang$virtual_dom$VirtualDom$attribute('min');
var _elm_lang$svg$Svg_Attributes$method = _elm_lang$virtual_dom$VirtualDom$attribute('method');
var _elm_lang$svg$Svg_Attributes$media = _elm_lang$virtual_dom$VirtualDom$attribute('media');
var _elm_lang$svg$Svg_Attributes$max = _elm_lang$virtual_dom$VirtualDom$attribute('max');
var _elm_lang$svg$Svg_Attributes$mathematical = _elm_lang$virtual_dom$VirtualDom$attribute('mathematical');
var _elm_lang$svg$Svg_Attributes$maskUnits = _elm_lang$virtual_dom$VirtualDom$attribute('maskUnits');
var _elm_lang$svg$Svg_Attributes$maskContentUnits = _elm_lang$virtual_dom$VirtualDom$attribute('maskContentUnits');
var _elm_lang$svg$Svg_Attributes$markerWidth = _elm_lang$virtual_dom$VirtualDom$attribute('markerWidth');
var _elm_lang$svg$Svg_Attributes$markerUnits = _elm_lang$virtual_dom$VirtualDom$attribute('markerUnits');
var _elm_lang$svg$Svg_Attributes$markerHeight = _elm_lang$virtual_dom$VirtualDom$attribute('markerHeight');
var _elm_lang$svg$Svg_Attributes$local = _elm_lang$virtual_dom$VirtualDom$attribute('local');
var _elm_lang$svg$Svg_Attributes$limitingConeAngle = _elm_lang$virtual_dom$VirtualDom$attribute('limitingConeAngle');
var _elm_lang$svg$Svg_Attributes$lengthAdjust = _elm_lang$virtual_dom$VirtualDom$attribute('lengthAdjust');
var _elm_lang$svg$Svg_Attributes$lang = _elm_lang$virtual_dom$VirtualDom$attribute('lang');
var _elm_lang$svg$Svg_Attributes$keyTimes = _elm_lang$virtual_dom$VirtualDom$attribute('keyTimes');
var _elm_lang$svg$Svg_Attributes$keySplines = _elm_lang$virtual_dom$VirtualDom$attribute('keySplines');
var _elm_lang$svg$Svg_Attributes$keyPoints = _elm_lang$virtual_dom$VirtualDom$attribute('keyPoints');
var _elm_lang$svg$Svg_Attributes$kernelUnitLength = _elm_lang$virtual_dom$VirtualDom$attribute('kernelUnitLength');
var _elm_lang$svg$Svg_Attributes$kernelMatrix = _elm_lang$virtual_dom$VirtualDom$attribute('kernelMatrix');
var _elm_lang$svg$Svg_Attributes$k4 = _elm_lang$virtual_dom$VirtualDom$attribute('k4');
var _elm_lang$svg$Svg_Attributes$k3 = _elm_lang$virtual_dom$VirtualDom$attribute('k3');
var _elm_lang$svg$Svg_Attributes$k2 = _elm_lang$virtual_dom$VirtualDom$attribute('k2');
var _elm_lang$svg$Svg_Attributes$k1 = _elm_lang$virtual_dom$VirtualDom$attribute('k1');
var _elm_lang$svg$Svg_Attributes$k = _elm_lang$virtual_dom$VirtualDom$attribute('k');
var _elm_lang$svg$Svg_Attributes$intercept = _elm_lang$virtual_dom$VirtualDom$attribute('intercept');
var _elm_lang$svg$Svg_Attributes$in2 = _elm_lang$virtual_dom$VirtualDom$attribute('in2');
var _elm_lang$svg$Svg_Attributes$in_ = _elm_lang$virtual_dom$VirtualDom$attribute('in');
var _elm_lang$svg$Svg_Attributes$ideographic = _elm_lang$virtual_dom$VirtualDom$attribute('ideographic');
var _elm_lang$svg$Svg_Attributes$id = _elm_lang$virtual_dom$VirtualDom$attribute('id');
var _elm_lang$svg$Svg_Attributes$horizOriginY = _elm_lang$virtual_dom$VirtualDom$attribute('horiz-origin-y');
var _elm_lang$svg$Svg_Attributes$horizOriginX = _elm_lang$virtual_dom$VirtualDom$attribute('horiz-origin-x');
var _elm_lang$svg$Svg_Attributes$horizAdvX = _elm_lang$virtual_dom$VirtualDom$attribute('horiz-adv-x');
var _elm_lang$svg$Svg_Attributes$height = _elm_lang$virtual_dom$VirtualDom$attribute('height');
var _elm_lang$svg$Svg_Attributes$hanging = _elm_lang$virtual_dom$VirtualDom$attribute('hanging');
var _elm_lang$svg$Svg_Attributes$gradientUnits = _elm_lang$virtual_dom$VirtualDom$attribute('gradientUnits');
var _elm_lang$svg$Svg_Attributes$gradientTransform = _elm_lang$virtual_dom$VirtualDom$attribute('gradientTransform');
var _elm_lang$svg$Svg_Attributes$glyphRef = _elm_lang$virtual_dom$VirtualDom$attribute('glyphRef');
var _elm_lang$svg$Svg_Attributes$glyphName = _elm_lang$virtual_dom$VirtualDom$attribute('glyph-name');
var _elm_lang$svg$Svg_Attributes$g2 = _elm_lang$virtual_dom$VirtualDom$attribute('g2');
var _elm_lang$svg$Svg_Attributes$g1 = _elm_lang$virtual_dom$VirtualDom$attribute('g1');
var _elm_lang$svg$Svg_Attributes$fy = _elm_lang$virtual_dom$VirtualDom$attribute('fy');
var _elm_lang$svg$Svg_Attributes$fx = _elm_lang$virtual_dom$VirtualDom$attribute('fx');
var _elm_lang$svg$Svg_Attributes$from = _elm_lang$virtual_dom$VirtualDom$attribute('from');
var _elm_lang$svg$Svg_Attributes$format = _elm_lang$virtual_dom$VirtualDom$attribute('format');
var _elm_lang$svg$Svg_Attributes$filterUnits = _elm_lang$virtual_dom$VirtualDom$attribute('filterUnits');
var _elm_lang$svg$Svg_Attributes$filterRes = _elm_lang$virtual_dom$VirtualDom$attribute('filterRes');
var _elm_lang$svg$Svg_Attributes$externalResourcesRequired = _elm_lang$virtual_dom$VirtualDom$attribute('externalResourcesRequired');
var _elm_lang$svg$Svg_Attributes$exponent = _elm_lang$virtual_dom$VirtualDom$attribute('exponent');
var _elm_lang$svg$Svg_Attributes$end = _elm_lang$virtual_dom$VirtualDom$attribute('end');
var _elm_lang$svg$Svg_Attributes$elevation = _elm_lang$virtual_dom$VirtualDom$attribute('elevation');
var _elm_lang$svg$Svg_Attributes$edgeMode = _elm_lang$virtual_dom$VirtualDom$attribute('edgeMode');
var _elm_lang$svg$Svg_Attributes$dy = _elm_lang$virtual_dom$VirtualDom$attribute('dy');
var _elm_lang$svg$Svg_Attributes$dx = _elm_lang$virtual_dom$VirtualDom$attribute('dx');
var _elm_lang$svg$Svg_Attributes$dur = _elm_lang$virtual_dom$VirtualDom$attribute('dur');
var _elm_lang$svg$Svg_Attributes$divisor = _elm_lang$virtual_dom$VirtualDom$attribute('divisor');
var _elm_lang$svg$Svg_Attributes$diffuseConstant = _elm_lang$virtual_dom$VirtualDom$attribute('diffuseConstant');
var _elm_lang$svg$Svg_Attributes$descent = _elm_lang$virtual_dom$VirtualDom$attribute('descent');
var _elm_lang$svg$Svg_Attributes$decelerate = _elm_lang$virtual_dom$VirtualDom$attribute('decelerate');
var _elm_lang$svg$Svg_Attributes$d = _elm_lang$virtual_dom$VirtualDom$attribute('d');
var _elm_lang$svg$Svg_Attributes$cy = _elm_lang$virtual_dom$VirtualDom$attribute('cy');
var _elm_lang$svg$Svg_Attributes$cx = _elm_lang$virtual_dom$VirtualDom$attribute('cx');
var _elm_lang$svg$Svg_Attributes$contentStyleType = _elm_lang$virtual_dom$VirtualDom$attribute('contentStyleType');
var _elm_lang$svg$Svg_Attributes$contentScriptType = _elm_lang$virtual_dom$VirtualDom$attribute('contentScriptType');
var _elm_lang$svg$Svg_Attributes$clipPathUnits = _elm_lang$virtual_dom$VirtualDom$attribute('clipPathUnits');
var _elm_lang$svg$Svg_Attributes$class = _elm_lang$virtual_dom$VirtualDom$attribute('class');
var _elm_lang$svg$Svg_Attributes$capHeight = _elm_lang$virtual_dom$VirtualDom$attribute('cap-height');
var _elm_lang$svg$Svg_Attributes$calcMode = _elm_lang$virtual_dom$VirtualDom$attribute('calcMode');
var _elm_lang$svg$Svg_Attributes$by = _elm_lang$virtual_dom$VirtualDom$attribute('by');
var _elm_lang$svg$Svg_Attributes$bias = _elm_lang$virtual_dom$VirtualDom$attribute('bias');
var _elm_lang$svg$Svg_Attributes$begin = _elm_lang$virtual_dom$VirtualDom$attribute('begin');
var _elm_lang$svg$Svg_Attributes$bbox = _elm_lang$virtual_dom$VirtualDom$attribute('bbox');
var _elm_lang$svg$Svg_Attributes$baseProfile = _elm_lang$virtual_dom$VirtualDom$attribute('baseProfile');
var _elm_lang$svg$Svg_Attributes$baseFrequency = _elm_lang$virtual_dom$VirtualDom$attribute('baseFrequency');
var _elm_lang$svg$Svg_Attributes$azimuth = _elm_lang$virtual_dom$VirtualDom$attribute('azimuth');
var _elm_lang$svg$Svg_Attributes$autoReverse = _elm_lang$virtual_dom$VirtualDom$attribute('autoReverse');
var _elm_lang$svg$Svg_Attributes$attributeType = _elm_lang$virtual_dom$VirtualDom$attribute('attributeType');
var _elm_lang$svg$Svg_Attributes$attributeName = _elm_lang$virtual_dom$VirtualDom$attribute('attributeName');
var _elm_lang$svg$Svg_Attributes$ascent = _elm_lang$virtual_dom$VirtualDom$attribute('ascent');
var _elm_lang$svg$Svg_Attributes$arabicForm = _elm_lang$virtual_dom$VirtualDom$attribute('arabic-form');
var _elm_lang$svg$Svg_Attributes$amplitude = _elm_lang$virtual_dom$VirtualDom$attribute('amplitude');
var _elm_lang$svg$Svg_Attributes$allowReorder = _elm_lang$virtual_dom$VirtualDom$attribute('allowReorder');
var _elm_lang$svg$Svg_Attributes$alphabetic = _elm_lang$virtual_dom$VirtualDom$attribute('alphabetic');
var _elm_lang$svg$Svg_Attributes$additive = _elm_lang$virtual_dom$VirtualDom$attribute('additive');
var _elm_lang$svg$Svg_Attributes$accumulate = _elm_lang$virtual_dom$VirtualDom$attribute('accumulate');
var _elm_lang$svg$Svg_Attributes$accelerate = _elm_lang$virtual_dom$VirtualDom$attribute('accelerate');
var _elm_lang$svg$Svg_Attributes$accentHeight = _elm_lang$virtual_dom$VirtualDom$attribute('accent-height');

var _elm_lang$window$Native_Window = function()
{

var size = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)	{
	callback(_elm_lang$core$Native_Scheduler.succeed({
		width: window.innerWidth,
		height: window.innerHeight
	}));
});

return {
	size: size
};

}();
var _elm_lang$window$Window_ops = _elm_lang$window$Window_ops || {};
_elm_lang$window$Window_ops['&>'] = F2(
	function (task1, task2) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (_p0) {
				return task2;
			},
			task1);
	});
var _elm_lang$window$Window$onSelfMsg = F3(
	function (router, dimensions, state) {
		var _p1 = state;
		if (_p1.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var send = function (_p2) {
				var _p3 = _p2;
				return A2(
					_elm_lang$core$Platform$sendToApp,
					router,
					_p3._0(dimensions));
			};
			return A2(
				_elm_lang$window$Window_ops['&>'],
				_elm_lang$core$Task$sequence(
					A2(_elm_lang$core$List$map, send, _p1._0.subs)),
				_elm_lang$core$Task$succeed(state));
		}
	});
var _elm_lang$window$Window$init = _elm_lang$core$Task$succeed(_elm_lang$core$Maybe$Nothing);
var _elm_lang$window$Window$size = _elm_lang$window$Native_Window.size;
var _elm_lang$window$Window$width = A2(
	_elm_lang$core$Task$map,
	function (_) {
		return _.width;
	},
	_elm_lang$window$Window$size);
var _elm_lang$window$Window$height = A2(
	_elm_lang$core$Task$map,
	function (_) {
		return _.height;
	},
	_elm_lang$window$Window$size);
var _elm_lang$window$Window$onEffects = F3(
	function (router, newSubs, oldState) {
		var _p4 = {ctor: '_Tuple2', _0: oldState, _1: newSubs};
		if (_p4._0.ctor === 'Nothing') {
			if (_p4._1.ctor === '[]') {
				return _elm_lang$core$Task$succeed(_elm_lang$core$Maybe$Nothing);
			} else {
				return A2(
					_elm_lang$core$Task$andThen,
					function (pid) {
						return _elm_lang$core$Task$succeed(
							_elm_lang$core$Maybe$Just(
								{subs: newSubs, pid: pid}));
					},
					_elm_lang$core$Process$spawn(
						A3(
							_elm_lang$dom$Dom_LowLevel$onWindow,
							'resize',
							_elm_lang$core$Json_Decode$succeed(
								{ctor: '_Tuple0'}),
							function (_p5) {
								return A2(
									_elm_lang$core$Task$andThen,
									_elm_lang$core$Platform$sendToSelf(router),
									_elm_lang$window$Window$size);
							})));
			}
		} else {
			if (_p4._1.ctor === '[]') {
				return A2(
					_elm_lang$window$Window_ops['&>'],
					_elm_lang$core$Process$kill(_p4._0._0.pid),
					_elm_lang$core$Task$succeed(_elm_lang$core$Maybe$Nothing));
			} else {
				return _elm_lang$core$Task$succeed(
					_elm_lang$core$Maybe$Just(
						{subs: newSubs, pid: _p4._0._0.pid}));
			}
		}
	});
var _elm_lang$window$Window$subscription = _elm_lang$core$Native_Platform.leaf('Window');
var _elm_lang$window$Window$Size = F2(
	function (a, b) {
		return {width: a, height: b};
	});
var _elm_lang$window$Window$MySub = function (a) {
	return {ctor: 'MySub', _0: a};
};
var _elm_lang$window$Window$resizes = function (tagger) {
	return _elm_lang$window$Window$subscription(
		_elm_lang$window$Window$MySub(tagger));
};
var _elm_lang$window$Window$subMap = F2(
	function (func, _p6) {
		var _p7 = _p6;
		return _elm_lang$window$Window$MySub(
			function (_p8) {
				return func(
					_p7._0(_p8));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Window'] = {pkg: 'elm-lang/window', init: _elm_lang$window$Window$init, onEffects: _elm_lang$window$Window$onEffects, onSelfMsg: _elm_lang$window$Window$onSelfMsg, tag: 'sub', subMap: _elm_lang$window$Window$subMap};

var _ohanhi$keyboard_extra$Keyboard_Extra$boolToInt = function (bool) {
	return bool ? 1 : 0;
};
var _ohanhi$keyboard_extra$Keyboard_Extra$remove = F2(
	function (code, list) {
		return A2(
			_elm_lang$core$List$filter,
			F2(
				function (x, y) {
					return !_elm_lang$core$Native_Utils.eq(x, y);
				})(code),
			list);
	});
var _ohanhi$keyboard_extra$Keyboard_Extra$insert = F2(
	function (code, list) {
		return A2(
			F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				}),
			code,
			A2(_ohanhi$keyboard_extra$Keyboard_Extra$remove, code, list));
	});
var _ohanhi$keyboard_extra$Keyboard_Extra$update = F2(
	function (msg, state) {
		var _p0 = msg;
		if (_p0.ctor === 'Down') {
			return A2(_ohanhi$keyboard_extra$Keyboard_Extra$insert, _p0._0, state);
		} else {
			return A2(_ohanhi$keyboard_extra$Keyboard_Extra$remove, _p0._0, state);
		}
	});
var _ohanhi$keyboard_extra$Keyboard_Extra$Arrows = F2(
	function (a, b) {
		return {x: a, y: b};
	});
var _ohanhi$keyboard_extra$Keyboard_Extra$Up = function (a) {
	return {ctor: 'Up', _0: a};
};
var _ohanhi$keyboard_extra$Keyboard_Extra$Down = function (a) {
	return {ctor: 'Down', _0: a};
};
var _ohanhi$keyboard_extra$Keyboard_Extra$KeyUp = function (a) {
	return {ctor: 'KeyUp', _0: a};
};
var _ohanhi$keyboard_extra$Keyboard_Extra$KeyDown = function (a) {
	return {ctor: 'KeyDown', _0: a};
};
var _ohanhi$keyboard_extra$Keyboard_Extra$updateWithKeyChange = F2(
	function (msg, state) {
		var _p1 = msg;
		if (_p1.ctor === 'Down') {
			var _p2 = _p1._0;
			var nextState = A2(_ohanhi$keyboard_extra$Keyboard_Extra$insert, _p2, state);
			var change = (!_elm_lang$core$Native_Utils.eq(
				_elm_lang$core$List$length(nextState),
				_elm_lang$core$List$length(state))) ? _elm_lang$core$Maybe$Just(
				_ohanhi$keyboard_extra$Keyboard_Extra$KeyDown(_p2)) : _elm_lang$core$Maybe$Nothing;
			return {ctor: '_Tuple2', _0: nextState, _1: change};
		} else {
			var _p3 = _p1._0;
			var nextState = A2(_ohanhi$keyboard_extra$Keyboard_Extra$remove, _p3, state);
			var change = (!_elm_lang$core$Native_Utils.eq(
				_elm_lang$core$List$length(nextState),
				_elm_lang$core$List$length(state))) ? _elm_lang$core$Maybe$Just(
				_ohanhi$keyboard_extra$Keyboard_Extra$KeyUp(_p3)) : _elm_lang$core$Maybe$Nothing;
			return {ctor: '_Tuple2', _0: nextState, _1: change};
		}
	});
var _ohanhi$keyboard_extra$Keyboard_Extra$NoDirection = {ctor: 'NoDirection'};
var _ohanhi$keyboard_extra$Keyboard_Extra$NorthWest = {ctor: 'NorthWest'};
var _ohanhi$keyboard_extra$Keyboard_Extra$West = {ctor: 'West'};
var _ohanhi$keyboard_extra$Keyboard_Extra$SouthWest = {ctor: 'SouthWest'};
var _ohanhi$keyboard_extra$Keyboard_Extra$South = {ctor: 'South'};
var _ohanhi$keyboard_extra$Keyboard_Extra$SouthEast = {ctor: 'SouthEast'};
var _ohanhi$keyboard_extra$Keyboard_Extra$East = {ctor: 'East'};
var _ohanhi$keyboard_extra$Keyboard_Extra$NorthEast = {ctor: 'NorthEast'};
var _ohanhi$keyboard_extra$Keyboard_Extra$North = {ctor: 'North'};
var _ohanhi$keyboard_extra$Keyboard_Extra$arrowsToDir = function (_p4) {
	var _p5 = _p4;
	var _p6 = {ctor: '_Tuple2', _0: _p5.x, _1: _p5.y};
	_v3_8:
	do {
		if (_p6.ctor === '_Tuple2') {
			switch (_p6._0) {
				case 1:
					switch (_p6._1) {
						case 1:
							return _ohanhi$keyboard_extra$Keyboard_Extra$NorthEast;
						case 0:
							return _ohanhi$keyboard_extra$Keyboard_Extra$East;
						case -1:
							return _ohanhi$keyboard_extra$Keyboard_Extra$SouthEast;
						default:
							break _v3_8;
					}
				case 0:
					switch (_p6._1) {
						case 1:
							return _ohanhi$keyboard_extra$Keyboard_Extra$North;
						case -1:
							return _ohanhi$keyboard_extra$Keyboard_Extra$South;
						default:
							break _v3_8;
					}
				case -1:
					switch (_p6._1) {
						case -1:
							return _ohanhi$keyboard_extra$Keyboard_Extra$SouthWest;
						case 0:
							return _ohanhi$keyboard_extra$Keyboard_Extra$West;
						case 1:
							return _ohanhi$keyboard_extra$Keyboard_Extra$NorthWest;
						default:
							break _v3_8;
					}
				default:
					break _v3_8;
			}
		} else {
			break _v3_8;
		}
	} while(false);
	return _ohanhi$keyboard_extra$Keyboard_Extra$NoDirection;
};
var _ohanhi$keyboard_extra$Keyboard_Extra$Other = {ctor: 'Other'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Altgr = {ctor: 'Altgr'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Meta = {ctor: 'Meta'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Quote = {ctor: 'Quote'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CloseBracket = {ctor: 'CloseBracket'};
var _ohanhi$keyboard_extra$Keyboard_Extra$BackSlash = {ctor: 'BackSlash'};
var _ohanhi$keyboard_extra$Keyboard_Extra$OpenBracket = {ctor: 'OpenBracket'};
var _ohanhi$keyboard_extra$Keyboard_Extra$BackQuote = {ctor: 'BackQuote'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Slash = {ctor: 'Slash'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Period = {ctor: 'Period'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Minus = {ctor: 'Minus'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Comma = {ctor: 'Comma'};
var _ohanhi$keyboard_extra$Keyboard_Extra$VolumeUp = {ctor: 'VolumeUp'};
var _ohanhi$keyboard_extra$Keyboard_Extra$VolumeDown = {ctor: 'VolumeDown'};
var _ohanhi$keyboard_extra$Keyboard_Extra$VolumeMute = {ctor: 'VolumeMute'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Tilde = {ctor: 'Tilde'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CloseCurlyBracket = {ctor: 'CloseCurlyBracket'};
var _ohanhi$keyboard_extra$Keyboard_Extra$OpenCurlyBracket = {ctor: 'OpenCurlyBracket'};
var _ohanhi$keyboard_extra$Keyboard_Extra$HyphenMinus = {ctor: 'HyphenMinus'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Pipe = {ctor: 'Pipe'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Plus = {ctor: 'Plus'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Asterisk = {ctor: 'Asterisk'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CloseParen = {ctor: 'CloseParen'};
var _ohanhi$keyboard_extra$Keyboard_Extra$OpenParen = {ctor: 'OpenParen'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Underscore = {ctor: 'Underscore'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Ampersand = {ctor: 'Ampersand'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Percent = {ctor: 'Percent'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Dollar = {ctor: 'Dollar'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Hash = {ctor: 'Hash'};
var _ohanhi$keyboard_extra$Keyboard_Extra$DoubleQuote = {ctor: 'DoubleQuote'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Exclamation = {ctor: 'Exclamation'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Circumflex = {ctor: 'Circumflex'};
var _ohanhi$keyboard_extra$Keyboard_Extra$ScrollLock = {ctor: 'ScrollLock'};
var _ohanhi$keyboard_extra$Keyboard_Extra$NumLock = {ctor: 'NumLock'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F24 = {ctor: 'F24'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F23 = {ctor: 'F23'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F22 = {ctor: 'F22'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F21 = {ctor: 'F21'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F20 = {ctor: 'F20'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F19 = {ctor: 'F19'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F18 = {ctor: 'F18'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F17 = {ctor: 'F17'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F16 = {ctor: 'F16'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F15 = {ctor: 'F15'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F14 = {ctor: 'F14'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F13 = {ctor: 'F13'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F12 = {ctor: 'F12'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F11 = {ctor: 'F11'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F10 = {ctor: 'F10'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F9 = {ctor: 'F9'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F8 = {ctor: 'F8'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F7 = {ctor: 'F7'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F6 = {ctor: 'F6'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F5 = {ctor: 'F5'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F4 = {ctor: 'F4'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F3 = {ctor: 'F3'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F2 = {ctor: 'F2'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F1 = {ctor: 'F1'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Divide = {ctor: 'Divide'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Decimal = {ctor: 'Decimal'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Subtract = {ctor: 'Subtract'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Separator = {ctor: 'Separator'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Add = {ctor: 'Add'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Multiply = {ctor: 'Multiply'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad9 = {ctor: 'Numpad9'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad8 = {ctor: 'Numpad8'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad7 = {ctor: 'Numpad7'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad6 = {ctor: 'Numpad6'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad5 = {ctor: 'Numpad5'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad4 = {ctor: 'Numpad4'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad3 = {ctor: 'Numpad3'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad2 = {ctor: 'Numpad2'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad1 = {ctor: 'Numpad1'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad0 = {ctor: 'Numpad0'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Sleep = {ctor: 'Sleep'};
var _ohanhi$keyboard_extra$Keyboard_Extra$ContextMenu = {ctor: 'ContextMenu'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Super = {ctor: 'Super'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharZ = {ctor: 'CharZ'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharY = {ctor: 'CharY'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharX = {ctor: 'CharX'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharW = {ctor: 'CharW'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharV = {ctor: 'CharV'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharU = {ctor: 'CharU'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharT = {ctor: 'CharT'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharS = {ctor: 'CharS'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharR = {ctor: 'CharR'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharQ = {ctor: 'CharQ'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharP = {ctor: 'CharP'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharO = {ctor: 'CharO'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharN = {ctor: 'CharN'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharM = {ctor: 'CharM'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharL = {ctor: 'CharL'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharK = {ctor: 'CharK'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharJ = {ctor: 'CharJ'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharI = {ctor: 'CharI'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharH = {ctor: 'CharH'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharG = {ctor: 'CharG'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharF = {ctor: 'CharF'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharE = {ctor: 'CharE'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharD = {ctor: 'CharD'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharC = {ctor: 'CharC'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharB = {ctor: 'CharB'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharA = {ctor: 'CharA'};
var _ohanhi$keyboard_extra$Keyboard_Extra$wasd = function (keys) {
	var toInt = function (key) {
		return _ohanhi$keyboard_extra$Keyboard_Extra$boolToInt(
			A2(_elm_lang$core$List$member, key, keys));
	};
	var x = toInt(_ohanhi$keyboard_extra$Keyboard_Extra$CharD) - toInt(_ohanhi$keyboard_extra$Keyboard_Extra$CharA);
	var y = toInt(_ohanhi$keyboard_extra$Keyboard_Extra$CharW) - toInt(_ohanhi$keyboard_extra$Keyboard_Extra$CharS);
	return {x: x, y: y};
};
var _ohanhi$keyboard_extra$Keyboard_Extra$wasdDirection = function (_p7) {
	return _ohanhi$keyboard_extra$Keyboard_Extra$arrowsToDir(
		_ohanhi$keyboard_extra$Keyboard_Extra$wasd(_p7));
};
var _ohanhi$keyboard_extra$Keyboard_Extra$At = {ctor: 'At'};
var _ohanhi$keyboard_extra$Keyboard_Extra$QuestionMark = {ctor: 'QuestionMark'};
var _ohanhi$keyboard_extra$Keyboard_Extra$GreaterThan = {ctor: 'GreaterThan'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Equals = {ctor: 'Equals'};
var _ohanhi$keyboard_extra$Keyboard_Extra$LessThan = {ctor: 'LessThan'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Semicolon = {ctor: 'Semicolon'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Colon = {ctor: 'Colon'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number9 = {ctor: 'Number9'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number8 = {ctor: 'Number8'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number7 = {ctor: 'Number7'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number6 = {ctor: 'Number6'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number5 = {ctor: 'Number5'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number4 = {ctor: 'Number4'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number3 = {ctor: 'Number3'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number2 = {ctor: 'Number2'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number1 = {ctor: 'Number1'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number0 = {ctor: 'Number0'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Delete = {ctor: 'Delete'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Insert = {ctor: 'Insert'};
var _ohanhi$keyboard_extra$Keyboard_Extra$PrintScreen = {ctor: 'PrintScreen'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Execute = {ctor: 'Execute'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Print = {ctor: 'Print'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Select = {ctor: 'Select'};
var _ohanhi$keyboard_extra$Keyboard_Extra$ArrowDown = {ctor: 'ArrowDown'};
var _ohanhi$keyboard_extra$Keyboard_Extra$ArrowRight = {ctor: 'ArrowRight'};
var _ohanhi$keyboard_extra$Keyboard_Extra$ArrowUp = {ctor: 'ArrowUp'};
var _ohanhi$keyboard_extra$Keyboard_Extra$ArrowLeft = {ctor: 'ArrowLeft'};
var _ohanhi$keyboard_extra$Keyboard_Extra$arrows = function (keys) {
	var toInt = function (key) {
		return _ohanhi$keyboard_extra$Keyboard_Extra$boolToInt(
			A2(_elm_lang$core$List$member, key, keys));
	};
	var x = toInt(_ohanhi$keyboard_extra$Keyboard_Extra$ArrowRight) - toInt(_ohanhi$keyboard_extra$Keyboard_Extra$ArrowLeft);
	var y = toInt(_ohanhi$keyboard_extra$Keyboard_Extra$ArrowUp) - toInt(_ohanhi$keyboard_extra$Keyboard_Extra$ArrowDown);
	return {x: x, y: y};
};
var _ohanhi$keyboard_extra$Keyboard_Extra$arrowsDirection = function (_p8) {
	return _ohanhi$keyboard_extra$Keyboard_Extra$arrowsToDir(
		_ohanhi$keyboard_extra$Keyboard_Extra$arrows(_p8));
};
var _ohanhi$keyboard_extra$Keyboard_Extra$Home = {ctor: 'Home'};
var _ohanhi$keyboard_extra$Keyboard_Extra$End = {ctor: 'End'};
var _ohanhi$keyboard_extra$Keyboard_Extra$PageDown = {ctor: 'PageDown'};
var _ohanhi$keyboard_extra$Keyboard_Extra$PageUp = {ctor: 'PageUp'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Space = {ctor: 'Space'};
var _ohanhi$keyboard_extra$Keyboard_Extra$ModeChange = {ctor: 'ModeChange'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Accept = {ctor: 'Accept'};
var _ohanhi$keyboard_extra$Keyboard_Extra$NonConvert = {ctor: 'NonConvert'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Convert = {ctor: 'Convert'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Escape = {ctor: 'Escape'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CapsLock = {ctor: 'CapsLock'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Pause = {ctor: 'Pause'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Alt = {ctor: 'Alt'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Control = {ctor: 'Control'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Shift = {ctor: 'Shift'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Enter = {ctor: 'Enter'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Clear = {ctor: 'Clear'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Tab = {ctor: 'Tab'};
var _ohanhi$keyboard_extra$Keyboard_Extra$BackSpace = {ctor: 'BackSpace'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Help = {ctor: 'Help'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Cancel = {ctor: 'Cancel'};
var _ohanhi$keyboard_extra$Keyboard_Extra$codeBook = {
	ctor: '::',
	_0: {ctor: '_Tuple2', _0: 3, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Cancel},
	_1: {
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: 6, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Help},
		_1: {
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: 8, _1: _ohanhi$keyboard_extra$Keyboard_Extra$BackSpace},
			_1: {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 9, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Tab},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 12, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Clear},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 13, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Enter},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 16, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Shift},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 17, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Control},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 18, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Alt},
									_1: {
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 19, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Pause},
										_1: {
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 20, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CapsLock},
											_1: {
												ctor: '::',
												_0: {ctor: '_Tuple2', _0: 27, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Escape},
												_1: {
													ctor: '::',
													_0: {ctor: '_Tuple2', _0: 28, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Convert},
													_1: {
														ctor: '::',
														_0: {ctor: '_Tuple2', _0: 29, _1: _ohanhi$keyboard_extra$Keyboard_Extra$NonConvert},
														_1: {
															ctor: '::',
															_0: {ctor: '_Tuple2', _0: 30, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Accept},
															_1: {
																ctor: '::',
																_0: {ctor: '_Tuple2', _0: 31, _1: _ohanhi$keyboard_extra$Keyboard_Extra$ModeChange},
																_1: {
																	ctor: '::',
																	_0: {ctor: '_Tuple2', _0: 32, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Space},
																	_1: {
																		ctor: '::',
																		_0: {ctor: '_Tuple2', _0: 33, _1: _ohanhi$keyboard_extra$Keyboard_Extra$PageUp},
																		_1: {
																			ctor: '::',
																			_0: {ctor: '_Tuple2', _0: 34, _1: _ohanhi$keyboard_extra$Keyboard_Extra$PageDown},
																			_1: {
																				ctor: '::',
																				_0: {ctor: '_Tuple2', _0: 35, _1: _ohanhi$keyboard_extra$Keyboard_Extra$End},
																				_1: {
																					ctor: '::',
																					_0: {ctor: '_Tuple2', _0: 36, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Home},
																					_1: {
																						ctor: '::',
																						_0: {ctor: '_Tuple2', _0: 37, _1: _ohanhi$keyboard_extra$Keyboard_Extra$ArrowLeft},
																						_1: {
																							ctor: '::',
																							_0: {ctor: '_Tuple2', _0: 38, _1: _ohanhi$keyboard_extra$Keyboard_Extra$ArrowUp},
																							_1: {
																								ctor: '::',
																								_0: {ctor: '_Tuple2', _0: 39, _1: _ohanhi$keyboard_extra$Keyboard_Extra$ArrowRight},
																								_1: {
																									ctor: '::',
																									_0: {ctor: '_Tuple2', _0: 40, _1: _ohanhi$keyboard_extra$Keyboard_Extra$ArrowDown},
																									_1: {
																										ctor: '::',
																										_0: {ctor: '_Tuple2', _0: 41, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Select},
																										_1: {
																											ctor: '::',
																											_0: {ctor: '_Tuple2', _0: 42, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Print},
																											_1: {
																												ctor: '::',
																												_0: {ctor: '_Tuple2', _0: 43, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Execute},
																												_1: {
																													ctor: '::',
																													_0: {ctor: '_Tuple2', _0: 44, _1: _ohanhi$keyboard_extra$Keyboard_Extra$PrintScreen},
																													_1: {
																														ctor: '::',
																														_0: {ctor: '_Tuple2', _0: 45, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Insert},
																														_1: {
																															ctor: '::',
																															_0: {ctor: '_Tuple2', _0: 46, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Delete},
																															_1: {
																																ctor: '::',
																																_0: {ctor: '_Tuple2', _0: 48, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number0},
																																_1: {
																																	ctor: '::',
																																	_0: {ctor: '_Tuple2', _0: 49, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number1},
																																	_1: {
																																		ctor: '::',
																																		_0: {ctor: '_Tuple2', _0: 50, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number2},
																																		_1: {
																																			ctor: '::',
																																			_0: {ctor: '_Tuple2', _0: 51, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number3},
																																			_1: {
																																				ctor: '::',
																																				_0: {ctor: '_Tuple2', _0: 52, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number4},
																																				_1: {
																																					ctor: '::',
																																					_0: {ctor: '_Tuple2', _0: 53, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number5},
																																					_1: {
																																						ctor: '::',
																																						_0: {ctor: '_Tuple2', _0: 54, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number6},
																																						_1: {
																																							ctor: '::',
																																							_0: {ctor: '_Tuple2', _0: 55, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number7},
																																							_1: {
																																								ctor: '::',
																																								_0: {ctor: '_Tuple2', _0: 56, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number8},
																																								_1: {
																																									ctor: '::',
																																									_0: {ctor: '_Tuple2', _0: 57, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number9},
																																									_1: {
																																										ctor: '::',
																																										_0: {ctor: '_Tuple2', _0: 58, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Colon},
																																										_1: {
																																											ctor: '::',
																																											_0: {ctor: '_Tuple2', _0: 59, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Semicolon},
																																											_1: {
																																												ctor: '::',
																																												_0: {ctor: '_Tuple2', _0: 60, _1: _ohanhi$keyboard_extra$Keyboard_Extra$LessThan},
																																												_1: {
																																													ctor: '::',
																																													_0: {ctor: '_Tuple2', _0: 61, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Equals},
																																													_1: {
																																														ctor: '::',
																																														_0: {ctor: '_Tuple2', _0: 62, _1: _ohanhi$keyboard_extra$Keyboard_Extra$GreaterThan},
																																														_1: {
																																															ctor: '::',
																																															_0: {ctor: '_Tuple2', _0: 63, _1: _ohanhi$keyboard_extra$Keyboard_Extra$QuestionMark},
																																															_1: {
																																																ctor: '::',
																																																_0: {ctor: '_Tuple2', _0: 64, _1: _ohanhi$keyboard_extra$Keyboard_Extra$At},
																																																_1: {
																																																	ctor: '::',
																																																	_0: {ctor: '_Tuple2', _0: 65, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharA},
																																																	_1: {
																																																		ctor: '::',
																																																		_0: {ctor: '_Tuple2', _0: 66, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharB},
																																																		_1: {
																																																			ctor: '::',
																																																			_0: {ctor: '_Tuple2', _0: 67, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharC},
																																																			_1: {
																																																				ctor: '::',
																																																				_0: {ctor: '_Tuple2', _0: 68, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharD},
																																																				_1: {
																																																					ctor: '::',
																																																					_0: {ctor: '_Tuple2', _0: 69, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharE},
																																																					_1: {
																																																						ctor: '::',
																																																						_0: {ctor: '_Tuple2', _0: 70, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharF},
																																																						_1: {
																																																							ctor: '::',
																																																							_0: {ctor: '_Tuple2', _0: 71, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharG},
																																																							_1: {
																																																								ctor: '::',
																																																								_0: {ctor: '_Tuple2', _0: 72, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharH},
																																																								_1: {
																																																									ctor: '::',
																																																									_0: {ctor: '_Tuple2', _0: 73, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharI},
																																																									_1: {
																																																										ctor: '::',
																																																										_0: {ctor: '_Tuple2', _0: 74, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharJ},
																																																										_1: {
																																																											ctor: '::',
																																																											_0: {ctor: '_Tuple2', _0: 75, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharK},
																																																											_1: {
																																																												ctor: '::',
																																																												_0: {ctor: '_Tuple2', _0: 76, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharL},
																																																												_1: {
																																																													ctor: '::',
																																																													_0: {ctor: '_Tuple2', _0: 77, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharM},
																																																													_1: {
																																																														ctor: '::',
																																																														_0: {ctor: '_Tuple2', _0: 78, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharN},
																																																														_1: {
																																																															ctor: '::',
																																																															_0: {ctor: '_Tuple2', _0: 79, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharO},
																																																															_1: {
																																																																ctor: '::',
																																																																_0: {ctor: '_Tuple2', _0: 80, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharP},
																																																																_1: {
																																																																	ctor: '::',
																																																																	_0: {ctor: '_Tuple2', _0: 81, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharQ},
																																																																	_1: {
																																																																		ctor: '::',
																																																																		_0: {ctor: '_Tuple2', _0: 82, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharR},
																																																																		_1: {
																																																																			ctor: '::',
																																																																			_0: {ctor: '_Tuple2', _0: 83, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharS},
																																																																			_1: {
																																																																				ctor: '::',
																																																																				_0: {ctor: '_Tuple2', _0: 84, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharT},
																																																																				_1: {
																																																																					ctor: '::',
																																																																					_0: {ctor: '_Tuple2', _0: 85, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharU},
																																																																					_1: {
																																																																						ctor: '::',
																																																																						_0: {ctor: '_Tuple2', _0: 86, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharV},
																																																																						_1: {
																																																																							ctor: '::',
																																																																							_0: {ctor: '_Tuple2', _0: 87, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharW},
																																																																							_1: {
																																																																								ctor: '::',
																																																																								_0: {ctor: '_Tuple2', _0: 88, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharX},
																																																																								_1: {
																																																																									ctor: '::',
																																																																									_0: {ctor: '_Tuple2', _0: 89, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharY},
																																																																									_1: {
																																																																										ctor: '::',
																																																																										_0: {ctor: '_Tuple2', _0: 90, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharZ},
																																																																										_1: {
																																																																											ctor: '::',
																																																																											_0: {ctor: '_Tuple2', _0: 91, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Super},
																																																																											_1: {
																																																																												ctor: '::',
																																																																												_0: {ctor: '_Tuple2', _0: 93, _1: _ohanhi$keyboard_extra$Keyboard_Extra$ContextMenu},
																																																																												_1: {
																																																																													ctor: '::',
																																																																													_0: {ctor: '_Tuple2', _0: 95, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Sleep},
																																																																													_1: {
																																																																														ctor: '::',
																																																																														_0: {ctor: '_Tuple2', _0: 96, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad0},
																																																																														_1: {
																																																																															ctor: '::',
																																																																															_0: {ctor: '_Tuple2', _0: 97, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad1},
																																																																															_1: {
																																																																																ctor: '::',
																																																																																_0: {ctor: '_Tuple2', _0: 98, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad2},
																																																																																_1: {
																																																																																	ctor: '::',
																																																																																	_0: {ctor: '_Tuple2', _0: 99, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad3},
																																																																																	_1: {
																																																																																		ctor: '::',
																																																																																		_0: {ctor: '_Tuple2', _0: 100, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad4},
																																																																																		_1: {
																																																																																			ctor: '::',
																																																																																			_0: {ctor: '_Tuple2', _0: 101, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad5},
																																																																																			_1: {
																																																																																				ctor: '::',
																																																																																				_0: {ctor: '_Tuple2', _0: 102, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad6},
																																																																																				_1: {
																																																																																					ctor: '::',
																																																																																					_0: {ctor: '_Tuple2', _0: 103, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad7},
																																																																																					_1: {
																																																																																						ctor: '::',
																																																																																						_0: {ctor: '_Tuple2', _0: 104, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad8},
																																																																																						_1: {
																																																																																							ctor: '::',
																																																																																							_0: {ctor: '_Tuple2', _0: 105, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad9},
																																																																																							_1: {
																																																																																								ctor: '::',
																																																																																								_0: {ctor: '_Tuple2', _0: 106, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Multiply},
																																																																																								_1: {
																																																																																									ctor: '::',
																																																																																									_0: {ctor: '_Tuple2', _0: 107, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Add},
																																																																																									_1: {
																																																																																										ctor: '::',
																																																																																										_0: {ctor: '_Tuple2', _0: 108, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Separator},
																																																																																										_1: {
																																																																																											ctor: '::',
																																																																																											_0: {ctor: '_Tuple2', _0: 109, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Subtract},
																																																																																											_1: {
																																																																																												ctor: '::',
																																																																																												_0: {ctor: '_Tuple2', _0: 110, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Decimal},
																																																																																												_1: {
																																																																																													ctor: '::',
																																																																																													_0: {ctor: '_Tuple2', _0: 111, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Divide},
																																																																																													_1: {
																																																																																														ctor: '::',
																																																																																														_0: {ctor: '_Tuple2', _0: 112, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F1},
																																																																																														_1: {
																																																																																															ctor: '::',
																																																																																															_0: {ctor: '_Tuple2', _0: 113, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F2},
																																																																																															_1: {
																																																																																																ctor: '::',
																																																																																																_0: {ctor: '_Tuple2', _0: 114, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F3},
																																																																																																_1: {
																																																																																																	ctor: '::',
																																																																																																	_0: {ctor: '_Tuple2', _0: 115, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F4},
																																																																																																	_1: {
																																																																																																		ctor: '::',
																																																																																																		_0: {ctor: '_Tuple2', _0: 116, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F5},
																																																																																																		_1: {
																																																																																																			ctor: '::',
																																																																																																			_0: {ctor: '_Tuple2', _0: 117, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F6},
																																																																																																			_1: {
																																																																																																				ctor: '::',
																																																																																																				_0: {ctor: '_Tuple2', _0: 118, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F7},
																																																																																																				_1: {
																																																																																																					ctor: '::',
																																																																																																					_0: {ctor: '_Tuple2', _0: 119, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F8},
																																																																																																					_1: {
																																																																																																						ctor: '::',
																																																																																																						_0: {ctor: '_Tuple2', _0: 120, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F9},
																																																																																																						_1: {
																																																																																																							ctor: '::',
																																																																																																							_0: {ctor: '_Tuple2', _0: 121, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F10},
																																																																																																							_1: {
																																																																																																								ctor: '::',
																																																																																																								_0: {ctor: '_Tuple2', _0: 122, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F11},
																																																																																																								_1: {
																																																																																																									ctor: '::',
																																																																																																									_0: {ctor: '_Tuple2', _0: 123, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F12},
																																																																																																									_1: {
																																																																																																										ctor: '::',
																																																																																																										_0: {ctor: '_Tuple2', _0: 124, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F13},
																																																																																																										_1: {
																																																																																																											ctor: '::',
																																																																																																											_0: {ctor: '_Tuple2', _0: 125, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F14},
																																																																																																											_1: {
																																																																																																												ctor: '::',
																																																																																																												_0: {ctor: '_Tuple2', _0: 126, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F15},
																																																																																																												_1: {
																																																																																																													ctor: '::',
																																																																																																													_0: {ctor: '_Tuple2', _0: 127, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F16},
																																																																																																													_1: {
																																																																																																														ctor: '::',
																																																																																																														_0: {ctor: '_Tuple2', _0: 128, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F17},
																																																																																																														_1: {
																																																																																																															ctor: '::',
																																																																																																															_0: {ctor: '_Tuple2', _0: 129, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F18},
																																																																																																															_1: {
																																																																																																																ctor: '::',
																																																																																																																_0: {ctor: '_Tuple2', _0: 130, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F19},
																																																																																																																_1: {
																																																																																																																	ctor: '::',
																																																																																																																	_0: {ctor: '_Tuple2', _0: 131, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F20},
																																																																																																																	_1: {
																																																																																																																		ctor: '::',
																																																																																																																		_0: {ctor: '_Tuple2', _0: 132, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F21},
																																																																																																																		_1: {
																																																																																																																			ctor: '::',
																																																																																																																			_0: {ctor: '_Tuple2', _0: 133, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F22},
																																																																																																																			_1: {
																																																																																																																				ctor: '::',
																																																																																																																				_0: {ctor: '_Tuple2', _0: 134, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F23},
																																																																																																																				_1: {
																																																																																																																					ctor: '::',
																																																																																																																					_0: {ctor: '_Tuple2', _0: 135, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F24},
																																																																																																																					_1: {
																																																																																																																						ctor: '::',
																																																																																																																						_0: {ctor: '_Tuple2', _0: 144, _1: _ohanhi$keyboard_extra$Keyboard_Extra$NumLock},
																																																																																																																						_1: {
																																																																																																																							ctor: '::',
																																																																																																																							_0: {ctor: '_Tuple2', _0: 145, _1: _ohanhi$keyboard_extra$Keyboard_Extra$ScrollLock},
																																																																																																																							_1: {
																																																																																																																								ctor: '::',
																																																																																																																								_0: {ctor: '_Tuple2', _0: 160, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Circumflex},
																																																																																																																								_1: {
																																																																																																																									ctor: '::',
																																																																																																																									_0: {ctor: '_Tuple2', _0: 161, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Exclamation},
																																																																																																																									_1: {
																																																																																																																										ctor: '::',
																																																																																																																										_0: {ctor: '_Tuple2', _0: 162, _1: _ohanhi$keyboard_extra$Keyboard_Extra$DoubleQuote},
																																																																																																																										_1: {
																																																																																																																											ctor: '::',
																																																																																																																											_0: {ctor: '_Tuple2', _0: 163, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Hash},
																																																																																																																											_1: {
																																																																																																																												ctor: '::',
																																																																																																																												_0: {ctor: '_Tuple2', _0: 164, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Dollar},
																																																																																																																												_1: {
																																																																																																																													ctor: '::',
																																																																																																																													_0: {ctor: '_Tuple2', _0: 165, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Percent},
																																																																																																																													_1: {
																																																																																																																														ctor: '::',
																																																																																																																														_0: {ctor: '_Tuple2', _0: 166, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Ampersand},
																																																																																																																														_1: {
																																																																																																																															ctor: '::',
																																																																																																																															_0: {ctor: '_Tuple2', _0: 167, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Underscore},
																																																																																																																															_1: {
																																																																																																																																ctor: '::',
																																																																																																																																_0: {ctor: '_Tuple2', _0: 168, _1: _ohanhi$keyboard_extra$Keyboard_Extra$OpenParen},
																																																																																																																																_1: {
																																																																																																																																	ctor: '::',
																																																																																																																																	_0: {ctor: '_Tuple2', _0: 169, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CloseParen},
																																																																																																																																	_1: {
																																																																																																																																		ctor: '::',
																																																																																																																																		_0: {ctor: '_Tuple2', _0: 170, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Asterisk},
																																																																																																																																		_1: {
																																																																																																																																			ctor: '::',
																																																																																																																																			_0: {ctor: '_Tuple2', _0: 171, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Plus},
																																																																																																																																			_1: {
																																																																																																																																				ctor: '::',
																																																																																																																																				_0: {ctor: '_Tuple2', _0: 172, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Pipe},
																																																																																																																																				_1: {
																																																																																																																																					ctor: '::',
																																																																																																																																					_0: {ctor: '_Tuple2', _0: 173, _1: _ohanhi$keyboard_extra$Keyboard_Extra$HyphenMinus},
																																																																																																																																					_1: {
																																																																																																																																						ctor: '::',
																																																																																																																																						_0: {ctor: '_Tuple2', _0: 174, _1: _ohanhi$keyboard_extra$Keyboard_Extra$OpenCurlyBracket},
																																																																																																																																						_1: {
																																																																																																																																							ctor: '::',
																																																																																																																																							_0: {ctor: '_Tuple2', _0: 175, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CloseCurlyBracket},
																																																																																																																																							_1: {
																																																																																																																																								ctor: '::',
																																																																																																																																								_0: {ctor: '_Tuple2', _0: 176, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Tilde},
																																																																																																																																								_1: {
																																																																																																																																									ctor: '::',
																																																																																																																																									_0: {ctor: '_Tuple2', _0: 181, _1: _ohanhi$keyboard_extra$Keyboard_Extra$VolumeMute},
																																																																																																																																									_1: {
																																																																																																																																										ctor: '::',
																																																																																																																																										_0: {ctor: '_Tuple2', _0: 182, _1: _ohanhi$keyboard_extra$Keyboard_Extra$VolumeDown},
																																																																																																																																										_1: {
																																																																																																																																											ctor: '::',
																																																																																																																																											_0: {ctor: '_Tuple2', _0: 183, _1: _ohanhi$keyboard_extra$Keyboard_Extra$VolumeUp},
																																																																																																																																											_1: {
																																																																																																																																												ctor: '::',
																																																																																																																																												_0: {ctor: '_Tuple2', _0: 186, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Semicolon},
																																																																																																																																												_1: {
																																																																																																																																													ctor: '::',
																																																																																																																																													_0: {ctor: '_Tuple2', _0: 187, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Equals},
																																																																																																																																													_1: {
																																																																																																																																														ctor: '::',
																																																																																																																																														_0: {ctor: '_Tuple2', _0: 188, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Comma},
																																																																																																																																														_1: {
																																																																																																																																															ctor: '::',
																																																																																																																																															_0: {ctor: '_Tuple2', _0: 189, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Minus},
																																																																																																																																															_1: {
																																																																																																																																																ctor: '::',
																																																																																																																																																_0: {ctor: '_Tuple2', _0: 190, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Period},
																																																																																																																																																_1: {
																																																																																																																																																	ctor: '::',
																																																																																																																																																	_0: {ctor: '_Tuple2', _0: 191, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Slash},
																																																																																																																																																	_1: {
																																																																																																																																																		ctor: '::',
																																																																																																																																																		_0: {ctor: '_Tuple2', _0: 192, _1: _ohanhi$keyboard_extra$Keyboard_Extra$BackQuote},
																																																																																																																																																		_1: {
																																																																																																																																																			ctor: '::',
																																																																																																																																																			_0: {ctor: '_Tuple2', _0: 219, _1: _ohanhi$keyboard_extra$Keyboard_Extra$OpenBracket},
																																																																																																																																																			_1: {
																																																																																																																																																				ctor: '::',
																																																																																																																																																				_0: {ctor: '_Tuple2', _0: 220, _1: _ohanhi$keyboard_extra$Keyboard_Extra$BackSlash},
																																																																																																																																																				_1: {
																																																																																																																																																					ctor: '::',
																																																																																																																																																					_0: {ctor: '_Tuple2', _0: 221, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CloseBracket},
																																																																																																																																																					_1: {
																																																																																																																																																						ctor: '::',
																																																																																																																																																						_0: {ctor: '_Tuple2', _0: 222, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Quote},
																																																																																																																																																						_1: {
																																																																																																																																																							ctor: '::',
																																																																																																																																																							_0: {ctor: '_Tuple2', _0: 224, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Meta},
																																																																																																																																																							_1: {
																																																																																																																																																								ctor: '::',
																																																																																																																																																								_0: {ctor: '_Tuple2', _0: 225, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Altgr},
																																																																																																																																																								_1: {ctor: '[]'}
																																																																																																																																																							}
																																																																																																																																																						}
																																																																																																																																																					}
																																																																																																																																																				}
																																																																																																																																																			}
																																																																																																																																																		}
																																																																																																																																																	}
																																																																																																																																																}
																																																																																																																																															}
																																																																																																																																														}
																																																																																																																																													}
																																																																																																																																												}
																																																																																																																																											}
																																																																																																																																										}
																																																																																																																																									}
																																																																																																																																								}
																																																																																																																																							}
																																																																																																																																						}
																																																																																																																																					}
																																																																																																																																				}
																																																																																																																																			}
																																																																																																																																		}
																																																																																																																																	}
																																																																																																																																}
																																																																																																																															}
																																																																																																																														}
																																																																																																																													}
																																																																																																																												}
																																																																																																																											}
																																																																																																																										}
																																																																																																																									}
																																																																																																																								}
																																																																																																																							}
																																																																																																																						}
																																																																																																																					}
																																																																																																																				}
																																																																																																																			}
																																																																																																																		}
																																																																																																																	}
																																																																																																																}
																																																																																																															}
																																																																																																														}
																																																																																																													}
																																																																																																												}
																																																																																																											}
																																																																																																										}
																																																																																																									}
																																																																																																								}
																																																																																																							}
																																																																																																						}
																																																																																																					}
																																																																																																				}
																																																																																																			}
																																																																																																		}
																																																																																																	}
																																																																																																}
																																																																																															}
																																																																																														}
																																																																																													}
																																																																																												}
																																																																																											}
																																																																																										}
																																																																																									}
																																																																																								}
																																																																																							}
																																																																																						}
																																																																																					}
																																																																																				}
																																																																																			}
																																																																																		}
																																																																																	}
																																																																																}
																																																																															}
																																																																														}
																																																																													}
																																																																												}
																																																																											}
																																																																										}
																																																																									}
																																																																								}
																																																																							}
																																																																						}
																																																																					}
																																																																				}
																																																																			}
																																																																		}
																																																																	}
																																																																}
																																																															}
																																																														}
																																																													}
																																																												}
																																																											}
																																																										}
																																																									}
																																																								}
																																																							}
																																																						}
																																																					}
																																																				}
																																																			}
																																																		}
																																																	}
																																																}
																																															}
																																														}
																																													}
																																												}
																																											}
																																										}
																																									}
																																								}
																																							}
																																						}
																																					}
																																				}
																																			}
																																		}
																																	}
																																}
																															}
																														}
																													}
																												}
																											}
																										}
																									}
																								}
																							}
																						}
																					}
																				}
																			}
																		}
																	}
																}
															}
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
};
var _ohanhi$keyboard_extra$Keyboard_Extra$toCode = function (key) {
	return A2(
		_elm_lang$core$Maybe$withDefault,
		0,
		_elm_lang$core$List$head(
			A2(
				_elm_lang$core$List$map,
				_elm_lang$core$Tuple$first,
				A2(
					_elm_lang$core$List$filter,
					function (_p9) {
						return A2(
							F2(
								function (x, y) {
									return _elm_lang$core$Native_Utils.eq(x, y);
								}),
							key,
							_elm_lang$core$Tuple$second(_p9));
					},
					_ohanhi$keyboard_extra$Keyboard_Extra$codeBook))));
};
var _ohanhi$keyboard_extra$Keyboard_Extra$codeDict = _elm_lang$core$Dict$fromList(_ohanhi$keyboard_extra$Keyboard_Extra$codeBook);
var _ohanhi$keyboard_extra$Keyboard_Extra$fromCode = function (code) {
	return A2(
		_elm_lang$core$Maybe$withDefault,
		_ohanhi$keyboard_extra$Keyboard_Extra$Other,
		A2(_elm_lang$core$Dict$get, code, _ohanhi$keyboard_extra$Keyboard_Extra$codeDict));
};
var _ohanhi$keyboard_extra$Keyboard_Extra$downs = function (toMsg) {
	return _elm_lang$keyboard$Keyboard$downs(
		function (_p10) {
			return toMsg(
				_ohanhi$keyboard_extra$Keyboard_Extra$fromCode(_p10));
		});
};
var _ohanhi$keyboard_extra$Keyboard_Extra$ups = function (toMsg) {
	return _elm_lang$keyboard$Keyboard$ups(
		function (_p11) {
			return toMsg(
				_ohanhi$keyboard_extra$Keyboard_Extra$fromCode(_p11));
		});
};
var _ohanhi$keyboard_extra$Keyboard_Extra$subscriptions = _elm_lang$core$Platform_Sub$batch(
	{
		ctor: '::',
		_0: _elm_lang$keyboard$Keyboard$downs(
			function (_p12) {
				return _ohanhi$keyboard_extra$Keyboard_Extra$Down(
					_ohanhi$keyboard_extra$Keyboard_Extra$fromCode(_p12));
			}),
		_1: {
			ctor: '::',
			_0: _elm_lang$keyboard$Keyboard$ups(
				function (_p13) {
					return _ohanhi$keyboard_extra$Keyboard_Extra$Up(
						_ohanhi$keyboard_extra$Keyboard_Extra$fromCode(_p13));
				}),
			_1: {ctor: '[]'}
		}
	});
var _ohanhi$keyboard_extra$Keyboard_Extra$targetKey = A2(
	_elm_lang$core$Json_Decode$map,
	_ohanhi$keyboard_extra$Keyboard_Extra$fromCode,
	A2(_elm_lang$core$Json_Decode$field, 'keyCode', _elm_lang$core$Json_Decode$int));

var _xarvh$elm_gamepad$Gamepad$estimateThreshold = function (_p0) {
	var _p1 = _p0;
	return (_elm_lang$core$Native_Utils.cmp(_p1._1, 0.5) < 0) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(_p1._0);
};
var _xarvh$elm_gamepad$Gamepad$boolToNumber = function (bool) {
	return bool ? 1 : 0;
};
var _xarvh$elm_gamepad$Gamepad$getIndex = function (_p2) {
	var _p3 = _p2;
	return _p3._1.index;
};
var _xarvh$elm_gamepad$Gamepad$unknownGetIndex = function (_p4) {
	var _p5 = _p4;
	return _p5._0.index;
};
var _xarvh$elm_gamepad$Gamepad$unknownGetId = function (_p6) {
	var _p7 = _p6;
	return _p7._0.id;
};
var _xarvh$elm_gamepad$Gamepad$reverseAxis = F2(
	function (isReverse, n) {
		return isReverse ? (0 - n) : n;
	});
var _xarvh$elm_gamepad$Gamepad$buttonToAxis = function (b) {
	return b ? 1 : 0;
};
var _xarvh$elm_gamepad$Gamepad$axisToButton = function (n) {
	return _elm_lang$core$Native_Utils.cmp(n, 0.1) > 0;
};
var _xarvh$elm_gamepad$Gamepad$maybeToReverse = function (maybeReverse) {
	var _p8 = maybeReverse;
	if ((_p8.ctor === 'Just') && (_p8._0 === '-')) {
		return true;
	} else {
		return false;
	}
};
var _xarvh$elm_gamepad$Gamepad$isConnected = function (rawGamepad) {
	return rawGamepad.connected && (_elm_lang$core$Native_Utils.cmp(rawGamepad.timestamp, 0) > 0);
};
var _xarvh$elm_gamepad$Gamepad$getRawGamepads = function (blob) {
	return A2(
		_elm_lang$core$List$filter,
		_xarvh$elm_gamepad$Gamepad$isConnected,
		A2(_elm_lang$core$List$filterMap, _elm_lang$core$Basics$identity, blob));
};
var _xarvh$elm_gamepad$Gamepad$buttonMapDivider = ',,,';
var _xarvh$elm_gamepad$Gamepad$databaseToString = function (_p9) {
	var _p10 = _p9;
	var tupleToString = function (_p11) {
		var _p12 = _p11;
		return A2(
			_elm_lang$core$Basics_ops['++'],
			_p12._0,
			A2(
				_elm_lang$core$Basics_ops['++'],
				_xarvh$elm_gamepad$Gamepad$buttonMapDivider,
				A2(_elm_lang$core$Basics_ops['++'], _p12._1, '\n')));
	};
	return A2(
		_elm_lang$core$String$join,
		'',
		A2(
			_elm_lang$core$List$sortBy,
			_elm_lang$core$Basics$identity,
			A2(
				_elm_lang$core$List$map,
				tupleToString,
				_elm_lang$core$Dict$toList(_p10._0))));
};
var _xarvh$elm_gamepad$Gamepad$intToString = _elm_lang$core$Basics$toString;
var _xarvh$elm_gamepad$Gamepad$destinationToString = function (destination) {
	var _p13 = destination;
	switch (_p13.ctor) {
		case 'A':
			return 'a';
		case 'B':
			return 'b';
		case 'X':
			return 'x';
		case 'Y':
			return 'y';
		case 'Start':
			return 'start';
		case 'Back':
			return 'back';
		case 'Home':
			return 'home';
		case 'LeftLeft':
			return 'leftleft';
		case 'LeftRight':
			return 'leftright';
		case 'LeftUp':
			return 'leftup';
		case 'LeftDown':
			return 'leftdown';
		case 'LeftStick':
			return 'leftstick';
		case 'LeftBumper':
			return 'leftbumper';
		case 'LeftTrigger':
			return 'lefttrigger';
		case 'RightLeft':
			return 'rightleft';
		case 'RightRight':
			return 'rightright';
		case 'RightUp':
			return 'rightup';
		case 'RightDown':
			return 'rightdown';
		case 'RightStick':
			return 'rightstick';
		case 'RightBumper':
			return 'rightbumper';
		case 'RightTrigger':
			return 'righttrigger';
		case 'DpadUp':
			return 'dpadup';
		case 'DpadDown':
			return 'dpaddown';
		case 'DpadLeft':
			return 'dpadleft';
		default:
			return 'dpadright';
	}
};
var _xarvh$elm_gamepad$Gamepad$fixAxisCoupling = F2(
	function (_p14, map) {
		var _p15 = _p14;
		var _p17 = _p15._0;
		var _p16 = {
			ctor: '_Tuple2',
			_0: A2(
				_elm_lang$core$Dict$get,
				_xarvh$elm_gamepad$Gamepad$destinationToString(_p17),
				map),
			_1: A2(
				_elm_lang$core$Dict$get,
				_xarvh$elm_gamepad$Gamepad$destinationToString(_p15._1),
				map)
		};
		if ((((_p16._0.ctor === 'Just') && (_p16._0._0._1.ctor === 'Axis')) && (_p16._1.ctor === 'Just')) && (_p16._1._0._1.ctor === 'Axis')) {
			return _elm_lang$core$Native_Utils.eq(_p16._0._0._2, _p16._1._0._2) ? A2(
				_elm_lang$core$Dict$remove,
				_xarvh$elm_gamepad$Gamepad$destinationToString(_p17),
				map) : map;
		} else {
			return map;
		}
	});
var _xarvh$elm_gamepad$Gamepad$RawGamepad = F7(
	function (a, b, c, d, e, f, g) {
		return {axes: a, buttons: b, connected: c, id: d, index: e, mapping: f, timestamp: g};
	});
var _xarvh$elm_gamepad$Gamepad$Gamepad = F2(
	function (a, b) {
		return {ctor: 'Gamepad', _0: a, _1: b};
	});
var _xarvh$elm_gamepad$Gamepad$UnknownGamepad = function (a) {
	return {ctor: 'UnknownGamepad', _0: a};
};
var _xarvh$elm_gamepad$Gamepad$getAllGamepadsAsUnknown = function (blob) {
	return A2(
		_elm_lang$core$List$map,
		_xarvh$elm_gamepad$Gamepad$UnknownGamepad,
		_xarvh$elm_gamepad$Gamepad$getRawGamepads(blob));
};
var _xarvh$elm_gamepad$Gamepad$Database = function (a) {
	return {ctor: 'Database', _0: a};
};
var _xarvh$elm_gamepad$Gamepad$emptyDatabase = _xarvh$elm_gamepad$Gamepad$Database(_elm_lang$core$Dict$empty);
var _xarvh$elm_gamepad$Gamepad$databaseFromString = function (databaseAsString) {
	var stringToTuple = function (dbEntry) {
		var _p18 = A2(_elm_lang$core$String$split, _xarvh$elm_gamepad$Gamepad$buttonMapDivider, dbEntry);
		if (((_p18.ctor === '::') && (_p18._1.ctor === '::')) && (_p18._1._1.ctor === '[]')) {
			return _elm_lang$core$Maybe$Just(
				{ctor: '_Tuple2', _0: _p18._0, _1: _p18._1._0});
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	};
	return _elm_lang$core$Result$Ok(
		_xarvh$elm_gamepad$Gamepad$Database(
			_elm_lang$core$Dict$fromList(
				A2(
					_elm_lang$core$List$filterMap,
					_elm_lang$core$Basics$identity,
					A2(
						_elm_lang$core$List$map,
						stringToTuple,
						A2(_elm_lang$core$String$split, '\n', databaseAsString))))));
};
var _xarvh$elm_gamepad$Gamepad$Origin = F3(
	function (a, b, c) {
		return {ctor: 'Origin', _0: a, _1: b, _2: c};
	});
var _xarvh$elm_gamepad$Gamepad$Button = {ctor: 'Button'};
var _xarvh$elm_gamepad$Gamepad$buttonToEstimate = F2(
	function (originIndex, _p19) {
		var _p20 = _p19;
		return {
			ctor: '_Tuple2',
			_0: A3(_xarvh$elm_gamepad$Gamepad$Origin, false, _xarvh$elm_gamepad$Gamepad$Button, originIndex),
			_1: _xarvh$elm_gamepad$Gamepad$boolToNumber(_p20._0)
		};
	});
var _xarvh$elm_gamepad$Gamepad$Axis = {ctor: 'Axis'};
var _xarvh$elm_gamepad$Gamepad$stringToInputType = function (s) {
	var _p21 = s;
	switch (_p21) {
		case 'a':
			return _elm_lang$core$Maybe$Just(_xarvh$elm_gamepad$Gamepad$Axis);
		case 'b':
			return _elm_lang$core$Maybe$Just(_xarvh$elm_gamepad$Gamepad$Button);
		default:
			return _elm_lang$core$Maybe$Nothing;
	}
};
var _xarvh$elm_gamepad$Gamepad$regexMatchToInputTuple = function (match) {
	var _p22 = match.submatches;
	if ((((((_p22.ctor === '::') && (_p22._1.ctor === '::')) && (_p22._1._1.ctor === '::')) && (_p22._1._1._0.ctor === 'Just')) && (_p22._1._1._1.ctor === '::')) && (_p22._1._1._1._0.ctor === 'Just')) {
		return A4(
			_elm_lang$core$Maybe$map3,
			F3(
				function (v0, v1, v2) {
					return {ctor: '_Tuple3', _0: v0, _1: v1, _2: v2};
				}),
			_xarvh$elm_gamepad$Gamepad$stringToInputType(_p22._1._1._0._0),
			_elm_lang$core$Result$toMaybe(
				_elm_lang$core$String$toInt(_p22._1._1._1._0._0)),
			_elm_lang$core$Maybe$Just(
				_xarvh$elm_gamepad$Gamepad$maybeToReverse(_p22._1._0)));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _xarvh$elm_gamepad$Gamepad$mappingToRawIndex = F2(
	function (destination, mapping) {
		var regex = A2(
			_elm_lang$core$Basics_ops['++'],
			'(^|,)',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_xarvh$elm_gamepad$Gamepad$destinationToString(destination),
				':(-)?([a-z]?)([0-9]+)(,|$)'));
		return A2(
			_elm_lang$core$Maybe$andThen,
			_xarvh$elm_gamepad$Gamepad$regexMatchToInputTuple,
			_elm_lang$core$List$head(
				A3(
					_elm_lang$core$Regex$find,
					_elm_lang$core$Regex$AtMost(1),
					_elm_lang$core$Regex$regex(regex),
					mapping)));
	});
var _xarvh$elm_gamepad$Gamepad$isPressed = F2(
	function (destination, _p23) {
		var _p24 = _p23;
		var _p26 = _p24._1;
		var _p25 = A2(_xarvh$elm_gamepad$Gamepad$mappingToRawIndex, destination, _p24._0);
		if (_p25.ctor === 'Nothing') {
			return false;
		} else {
			if (_p25._0._0.ctor === 'Axis') {
				return _xarvh$elm_gamepad$Gamepad$axisToButton(
					A2(
						_xarvh$elm_gamepad$Gamepad$reverseAxis,
						_p25._0._2,
						A2(
							_elm_lang$core$Maybe$withDefault,
							0,
							A2(_elm_lang$core$Array$get, _p25._0._1, _p26.axes))));
			} else {
				return A2(
					_elm_lang$core$Maybe$withDefault,
					false,
					A2(
						_elm_lang$core$Maybe$map,
						_elm_lang$core$Tuple$first,
						A2(_elm_lang$core$Array$get, _p25._0._1, _p26.buttons)));
			}
		}
	});
var _xarvh$elm_gamepad$Gamepad$getValue = F2(
	function (destination, _p27) {
		var _p28 = _p27;
		var _p30 = _p28._1;
		var _p29 = A2(_xarvh$elm_gamepad$Gamepad$mappingToRawIndex, destination, _p28._0);
		if (_p29.ctor === 'Nothing') {
			return 0;
		} else {
			if (_p29._0._0.ctor === 'Axis') {
				return A2(
					_xarvh$elm_gamepad$Gamepad$reverseAxis,
					_p29._0._2,
					A2(
						_elm_lang$core$Maybe$withDefault,
						0,
						A2(_elm_lang$core$Array$get, _p29._0._1, _p30.axes)));
			} else {
				return A2(
					_elm_lang$core$Maybe$withDefault,
					0,
					A2(
						_elm_lang$core$Maybe$map,
						_elm_lang$core$Tuple$second,
						A2(_elm_lang$core$Array$get, _p29._0._1, _p30.buttons)));
			}
		}
	});
var _xarvh$elm_gamepad$Gamepad$getAxis = F3(
	function (negativeDestination, positiveDestination, pad) {
		return A3(
			_elm_lang$core$Basics$clamp,
			-1,
			1,
			A2(_xarvh$elm_gamepad$Gamepad$getValue, positiveDestination, pad) - A2(_xarvh$elm_gamepad$Gamepad$getValue, negativeDestination, pad));
	});
var _xarvh$elm_gamepad$Gamepad$axisToEstimate = F2(
	function (originIndex, value) {
		return {
			ctor: '_Tuple2',
			_0: A3(
				_xarvh$elm_gamepad$Gamepad$Origin,
				_elm_lang$core$Native_Utils.cmp(value, 0) < 0,
				_xarvh$elm_gamepad$Gamepad$Axis,
				originIndex),
			_1: _elm_lang$core$Basics$abs(value)
		};
	});
var _xarvh$elm_gamepad$Gamepad$estimateOrigin = function (_p31) {
	var _p32 = _p31;
	var _p33 = _p32._0;
	var buttonsEstimates = A2(_elm_lang$core$Array$indexedMap, _xarvh$elm_gamepad$Gamepad$buttonToEstimate, _p33.buttons);
	var axesEstimates = A2(_elm_lang$core$Array$indexedMap, _xarvh$elm_gamepad$Gamepad$axisToEstimate, _p33.axes);
	return A2(
		_elm_lang$core$Maybe$andThen,
		_xarvh$elm_gamepad$Gamepad$estimateThreshold,
		_elm_lang$core$List$head(
			_elm_lang$core$List$reverse(
				A2(
					_elm_lang$core$List$sortBy,
					_elm_lang$core$Tuple$second,
					_elm_lang$core$Array$toList(
						A2(_elm_lang$core$Array$append, axesEstimates, buttonsEstimates))))));
};
var _xarvh$elm_gamepad$Gamepad$DpadRight = {ctor: 'DpadRight'};
var _xarvh$elm_gamepad$Gamepad$dpadRightIsPressed = _xarvh$elm_gamepad$Gamepad$isPressed(_xarvh$elm_gamepad$Gamepad$DpadRight);
var _xarvh$elm_gamepad$Gamepad$DpadLeft = {ctor: 'DpadLeft'};
var _xarvh$elm_gamepad$Gamepad$dpadLeftIsPressed = _xarvh$elm_gamepad$Gamepad$isPressed(_xarvh$elm_gamepad$Gamepad$DpadLeft);
var _xarvh$elm_gamepad$Gamepad$dpadX = function (pad) {
	return _xarvh$elm_gamepad$Gamepad$dpadLeftIsPressed(pad) ? -1 : (_xarvh$elm_gamepad$Gamepad$dpadRightIsPressed(pad) ? 1 : 0);
};
var _xarvh$elm_gamepad$Gamepad$DpadDown = {ctor: 'DpadDown'};
var _xarvh$elm_gamepad$Gamepad$dpadDownIsPressed = _xarvh$elm_gamepad$Gamepad$isPressed(_xarvh$elm_gamepad$Gamepad$DpadDown);
var _xarvh$elm_gamepad$Gamepad$DpadUp = {ctor: 'DpadUp'};
var _xarvh$elm_gamepad$Gamepad$dpadUpIsPressed = _xarvh$elm_gamepad$Gamepad$isPressed(_xarvh$elm_gamepad$Gamepad$DpadUp);
var _xarvh$elm_gamepad$Gamepad$dpadY = function (pad) {
	return _xarvh$elm_gamepad$Gamepad$dpadUpIsPressed(pad) ? 1 : (_xarvh$elm_gamepad$Gamepad$dpadDownIsPressed(pad) ? -1 : 0);
};
var _xarvh$elm_gamepad$Gamepad$RightTrigger = {ctor: 'RightTrigger'};
var _xarvh$elm_gamepad$Gamepad$rightTriggerIsPressed = _xarvh$elm_gamepad$Gamepad$isPressed(_xarvh$elm_gamepad$Gamepad$RightTrigger);
var _xarvh$elm_gamepad$Gamepad$rightTriggerValue = _xarvh$elm_gamepad$Gamepad$getValue(_xarvh$elm_gamepad$Gamepad$RightTrigger);
var _xarvh$elm_gamepad$Gamepad$RightBumper = {ctor: 'RightBumper'};
var _xarvh$elm_gamepad$Gamepad$rightBumperIsPressed = _xarvh$elm_gamepad$Gamepad$isPressed(_xarvh$elm_gamepad$Gamepad$RightBumper);
var _xarvh$elm_gamepad$Gamepad$RightStick = {ctor: 'RightStick'};
var _xarvh$elm_gamepad$Gamepad$rightStickIsPressed = _xarvh$elm_gamepad$Gamepad$isPressed(_xarvh$elm_gamepad$Gamepad$RightStick);
var _xarvh$elm_gamepad$Gamepad$RightDown = {ctor: 'RightDown'};
var _xarvh$elm_gamepad$Gamepad$RightUp = {ctor: 'RightUp'};
var _xarvh$elm_gamepad$Gamepad$rightY = A2(_xarvh$elm_gamepad$Gamepad$getAxis, _xarvh$elm_gamepad$Gamepad$RightDown, _xarvh$elm_gamepad$Gamepad$RightUp);
var _xarvh$elm_gamepad$Gamepad$RightRight = {ctor: 'RightRight'};
var _xarvh$elm_gamepad$Gamepad$RightLeft = {ctor: 'RightLeft'};
var _xarvh$elm_gamepad$Gamepad$rightX = A2(_xarvh$elm_gamepad$Gamepad$getAxis, _xarvh$elm_gamepad$Gamepad$RightLeft, _xarvh$elm_gamepad$Gamepad$RightRight);
var _xarvh$elm_gamepad$Gamepad$LeftTrigger = {ctor: 'LeftTrigger'};
var _xarvh$elm_gamepad$Gamepad$leftTriggerIsPressed = _xarvh$elm_gamepad$Gamepad$isPressed(_xarvh$elm_gamepad$Gamepad$LeftTrigger);
var _xarvh$elm_gamepad$Gamepad$leftTriggerValue = _xarvh$elm_gamepad$Gamepad$getValue(_xarvh$elm_gamepad$Gamepad$LeftTrigger);
var _xarvh$elm_gamepad$Gamepad$LeftBumper = {ctor: 'LeftBumper'};
var _xarvh$elm_gamepad$Gamepad$leftBumperIsPressed = _xarvh$elm_gamepad$Gamepad$isPressed(_xarvh$elm_gamepad$Gamepad$LeftBumper);
var _xarvh$elm_gamepad$Gamepad$LeftStick = {ctor: 'LeftStick'};
var _xarvh$elm_gamepad$Gamepad$leftStickIsPressed = _xarvh$elm_gamepad$Gamepad$isPressed(_xarvh$elm_gamepad$Gamepad$LeftStick);
var _xarvh$elm_gamepad$Gamepad$LeftDown = {ctor: 'LeftDown'};
var _xarvh$elm_gamepad$Gamepad$LeftUp = {ctor: 'LeftUp'};
var _xarvh$elm_gamepad$Gamepad$leftY = A2(_xarvh$elm_gamepad$Gamepad$getAxis, _xarvh$elm_gamepad$Gamepad$LeftDown, _xarvh$elm_gamepad$Gamepad$LeftUp);
var _xarvh$elm_gamepad$Gamepad$LeftRight = {ctor: 'LeftRight'};
var _xarvh$elm_gamepad$Gamepad$LeftLeft = {ctor: 'LeftLeft'};
var _xarvh$elm_gamepad$Gamepad$fixAllAxesCoupling = function (map) {
	return _elm_lang$core$Dict$toList(
		A3(
			_elm_lang$core$List$foldr,
			_xarvh$elm_gamepad$Gamepad$fixAxisCoupling,
			_elm_lang$core$Dict$fromList(map),
			{
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: _xarvh$elm_gamepad$Gamepad$LeftLeft, _1: _xarvh$elm_gamepad$Gamepad$LeftRight},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: _xarvh$elm_gamepad$Gamepad$LeftUp, _1: _xarvh$elm_gamepad$Gamepad$LeftDown},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: _xarvh$elm_gamepad$Gamepad$RightLeft, _1: _xarvh$elm_gamepad$Gamepad$RightRight},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: _xarvh$elm_gamepad$Gamepad$RightUp, _1: _xarvh$elm_gamepad$Gamepad$RightDown},
							_1: {ctor: '[]'}
						}
					}
				}
			}));
};
var _xarvh$elm_gamepad$Gamepad$buttonMap = function (map) {
	var tupleDestinationToString = function (_p34) {
		var _p35 = _p34;
		return {
			ctor: '_Tuple2',
			_0: _xarvh$elm_gamepad$Gamepad$destinationToString(_p35._0),
			_1: _p35._1
		};
	};
	var typeToString = function (originType) {
		var _p36 = originType;
		if (_p36.ctor === 'Axis') {
			return 'a';
		} else {
			return 'b';
		}
	};
	var hasMinus = function (isReverse) {
		return isReverse ? '-' : '';
	};
	var originToCode = function (_p37) {
		var _p38 = _p37;
		return A2(
			_elm_lang$core$Basics_ops['++'],
			hasMinus(_p38._0),
			A2(
				_elm_lang$core$Basics_ops['++'],
				typeToString(_p38._1),
				_xarvh$elm_gamepad$Gamepad$intToString(_p38._2)));
	};
	var tupleToString = function (_p39) {
		var _p40 = _p39;
		return A2(
			_elm_lang$core$Basics_ops['++'],
			_p40._0,
			A2(
				_elm_lang$core$Basics_ops['++'],
				':',
				originToCode(_p40._1)));
	};
	return A2(
		_elm_lang$core$String$join,
		',',
		A2(
			_elm_lang$core$List$sortBy,
			_elm_lang$core$Basics$identity,
			A2(
				_elm_lang$core$List$map,
				tupleToString,
				_xarvh$elm_gamepad$Gamepad$fixAllAxesCoupling(
					A2(_elm_lang$core$List$map, tupleDestinationToString, map)))));
};
var _xarvh$elm_gamepad$Gamepad$buttonMapToUpdateDatabase = F3(
	function (unknownGamepad, map, _p41) {
		var _p42 = _p41;
		return _xarvh$elm_gamepad$Gamepad$Database(
			A3(
				_elm_lang$core$Dict$insert,
				_xarvh$elm_gamepad$Gamepad$unknownGetId(unknownGamepad),
				_xarvh$elm_gamepad$Gamepad$buttonMap(map),
				_p42._0));
	});
var _xarvh$elm_gamepad$Gamepad$leftX = A2(_xarvh$elm_gamepad$Gamepad$getAxis, _xarvh$elm_gamepad$Gamepad$LeftLeft, _xarvh$elm_gamepad$Gamepad$LeftRight);
var _xarvh$elm_gamepad$Gamepad$Home = {ctor: 'Home'};
var _xarvh$elm_gamepad$Gamepad$homeIsPressed = _xarvh$elm_gamepad$Gamepad$isPressed(_xarvh$elm_gamepad$Gamepad$Home);
var _xarvh$elm_gamepad$Gamepad$Back = {ctor: 'Back'};
var _xarvh$elm_gamepad$Gamepad$backIsPressed = _xarvh$elm_gamepad$Gamepad$isPressed(_xarvh$elm_gamepad$Gamepad$Back);
var _xarvh$elm_gamepad$Gamepad$Start = {ctor: 'Start'};
var _xarvh$elm_gamepad$Gamepad$startIsPressed = _xarvh$elm_gamepad$Gamepad$isPressed(_xarvh$elm_gamepad$Gamepad$Start);
var _xarvh$elm_gamepad$Gamepad$Y = {ctor: 'Y'};
var _xarvh$elm_gamepad$Gamepad$yIsPressed = _xarvh$elm_gamepad$Gamepad$isPressed(_xarvh$elm_gamepad$Gamepad$Y);
var _xarvh$elm_gamepad$Gamepad$X = {ctor: 'X'};
var _xarvh$elm_gamepad$Gamepad$xIsPressed = _xarvh$elm_gamepad$Gamepad$isPressed(_xarvh$elm_gamepad$Gamepad$X);
var _xarvh$elm_gamepad$Gamepad$B = {ctor: 'B'};
var _xarvh$elm_gamepad$Gamepad$bIsPressed = _xarvh$elm_gamepad$Gamepad$isPressed(_xarvh$elm_gamepad$Gamepad$B);
var _xarvh$elm_gamepad$Gamepad$A = {ctor: 'A'};
var _xarvh$elm_gamepad$Gamepad$standardButtonMaps = _elm_lang$core$Dict$fromList(
	{
		ctor: '::',
		_0: {
			ctor: '_Tuple2',
			_0: 'standard',
			_1: _xarvh$elm_gamepad$Gamepad$buttonMap(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: _xarvh$elm_gamepad$Gamepad$A,
						_1: A3(_xarvh$elm_gamepad$Gamepad$Origin, false, _xarvh$elm_gamepad$Gamepad$Button, 0)
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: _xarvh$elm_gamepad$Gamepad$B,
							_1: A3(_xarvh$elm_gamepad$Gamepad$Origin, false, _xarvh$elm_gamepad$Gamepad$Button, 1)
						},
						_1: {
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: _xarvh$elm_gamepad$Gamepad$X,
								_1: A3(_xarvh$elm_gamepad$Gamepad$Origin, false, _xarvh$elm_gamepad$Gamepad$Button, 2)
							},
							_1: {
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: _xarvh$elm_gamepad$Gamepad$Y,
									_1: A3(_xarvh$elm_gamepad$Gamepad$Origin, false, _xarvh$elm_gamepad$Gamepad$Button, 3)
								},
								_1: {
									ctor: '::',
									_0: {
										ctor: '_Tuple2',
										_0: _xarvh$elm_gamepad$Gamepad$Start,
										_1: A3(_xarvh$elm_gamepad$Gamepad$Origin, false, _xarvh$elm_gamepad$Gamepad$Button, 9)
									},
									_1: {
										ctor: '::',
										_0: {
											ctor: '_Tuple2',
											_0: _xarvh$elm_gamepad$Gamepad$Back,
											_1: A3(_xarvh$elm_gamepad$Gamepad$Origin, false, _xarvh$elm_gamepad$Gamepad$Button, 8)
										},
										_1: {
											ctor: '::',
											_0: {
												ctor: '_Tuple2',
												_0: _xarvh$elm_gamepad$Gamepad$Home,
												_1: A3(_xarvh$elm_gamepad$Gamepad$Origin, false, _xarvh$elm_gamepad$Gamepad$Button, 16)
											},
											_1: {
												ctor: '::',
												_0: {
													ctor: '_Tuple2',
													_0: _xarvh$elm_gamepad$Gamepad$LeftRight,
													_1: A3(_xarvh$elm_gamepad$Gamepad$Origin, false, _xarvh$elm_gamepad$Gamepad$Axis, 0)
												},
												_1: {
													ctor: '::',
													_0: {
														ctor: '_Tuple2',
														_0: _xarvh$elm_gamepad$Gamepad$LeftDown,
														_1: A3(_xarvh$elm_gamepad$Gamepad$Origin, false, _xarvh$elm_gamepad$Gamepad$Axis, 1)
													},
													_1: {
														ctor: '::',
														_0: {
															ctor: '_Tuple2',
															_0: _xarvh$elm_gamepad$Gamepad$LeftStick,
															_1: A3(_xarvh$elm_gamepad$Gamepad$Origin, false, _xarvh$elm_gamepad$Gamepad$Button, 10)
														},
														_1: {
															ctor: '::',
															_0: {
																ctor: '_Tuple2',
																_0: _xarvh$elm_gamepad$Gamepad$LeftBumper,
																_1: A3(_xarvh$elm_gamepad$Gamepad$Origin, false, _xarvh$elm_gamepad$Gamepad$Button, 4)
															},
															_1: {
																ctor: '::',
																_0: {
																	ctor: '_Tuple2',
																	_0: _xarvh$elm_gamepad$Gamepad$LeftTrigger,
																	_1: A3(_xarvh$elm_gamepad$Gamepad$Origin, false, _xarvh$elm_gamepad$Gamepad$Button, 6)
																},
																_1: {
																	ctor: '::',
																	_0: {
																		ctor: '_Tuple2',
																		_0: _xarvh$elm_gamepad$Gamepad$RightRight,
																		_1: A3(_xarvh$elm_gamepad$Gamepad$Origin, false, _xarvh$elm_gamepad$Gamepad$Axis, 2)
																	},
																	_1: {
																		ctor: '::',
																		_0: {
																			ctor: '_Tuple2',
																			_0: _xarvh$elm_gamepad$Gamepad$RightDown,
																			_1: A3(_xarvh$elm_gamepad$Gamepad$Origin, false, _xarvh$elm_gamepad$Gamepad$Axis, 3)
																		},
																		_1: {
																			ctor: '::',
																			_0: {
																				ctor: '_Tuple2',
																				_0: _xarvh$elm_gamepad$Gamepad$RightStick,
																				_1: A3(_xarvh$elm_gamepad$Gamepad$Origin, false, _xarvh$elm_gamepad$Gamepad$Button, 11)
																			},
																			_1: {
																				ctor: '::',
																				_0: {
																					ctor: '_Tuple2',
																					_0: _xarvh$elm_gamepad$Gamepad$RightBumper,
																					_1: A3(_xarvh$elm_gamepad$Gamepad$Origin, false, _xarvh$elm_gamepad$Gamepad$Button, 5)
																				},
																				_1: {
																					ctor: '::',
																					_0: {
																						ctor: '_Tuple2',
																						_0: _xarvh$elm_gamepad$Gamepad$RightTrigger,
																						_1: A3(_xarvh$elm_gamepad$Gamepad$Origin, false, _xarvh$elm_gamepad$Gamepad$Button, 7)
																					},
																					_1: {
																						ctor: '::',
																						_0: {
																							ctor: '_Tuple2',
																							_0: _xarvh$elm_gamepad$Gamepad$DpadUp,
																							_1: A3(_xarvh$elm_gamepad$Gamepad$Origin, false, _xarvh$elm_gamepad$Gamepad$Button, 12)
																						},
																						_1: {
																							ctor: '::',
																							_0: {
																								ctor: '_Tuple2',
																								_0: _xarvh$elm_gamepad$Gamepad$DpadDown,
																								_1: A3(_xarvh$elm_gamepad$Gamepad$Origin, false, _xarvh$elm_gamepad$Gamepad$Button, 13)
																							},
																							_1: {
																								ctor: '::',
																								_0: {
																									ctor: '_Tuple2',
																									_0: _xarvh$elm_gamepad$Gamepad$DpadLeft,
																									_1: A3(_xarvh$elm_gamepad$Gamepad$Origin, false, _xarvh$elm_gamepad$Gamepad$Button, 14)
																								},
																								_1: {
																									ctor: '::',
																									_0: {
																										ctor: '_Tuple2',
																										_0: _xarvh$elm_gamepad$Gamepad$DpadRight,
																										_1: A3(_xarvh$elm_gamepad$Gamepad$Origin, false, _xarvh$elm_gamepad$Gamepad$Button, 15)
																									},
																									_1: {ctor: '[]'}
																								}
																							}
																						}
																					}
																				}
																			}
																		}
																	}
																}
															}
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				})
		},
		_1: {ctor: '[]'}
	});
var _xarvh$elm_gamepad$Gamepad$getGamepadButtonMap = F2(
	function (_p43, rawGamepad) {
		var _p44 = _p43;
		var _p45 = A2(_elm_lang$core$Dict$get, rawGamepad.id, _p44._0);
		if (_p45.ctor === 'Just') {
			return _elm_lang$core$Maybe$Just(_p45._0);
		} else {
			return A2(_elm_lang$core$Dict$get, rawGamepad.mapping, _xarvh$elm_gamepad$Gamepad$standardButtonMaps);
		}
	});
var _xarvh$elm_gamepad$Gamepad$getKnownAndUnknownGamepads = F2(
	function (database, blob) {
		var foldRawGamepad = F2(
			function (rawGamepad, _p46) {
				var _p47 = _p46;
				var _p50 = _p47._1;
				var _p49 = _p47._0;
				var _p48 = A2(_xarvh$elm_gamepad$Gamepad$getGamepadButtonMap, database, rawGamepad);
				if (_p48.ctor === 'Nothing') {
					return {
						ctor: '_Tuple2',
						_0: _p49,
						_1: {
							ctor: '::',
							_0: _xarvh$elm_gamepad$Gamepad$UnknownGamepad(rawGamepad),
							_1: _p50
						}
					};
				} else {
					return {
						ctor: '_Tuple2',
						_0: {
							ctor: '::',
							_0: A2(_xarvh$elm_gamepad$Gamepad$Gamepad, _p48._0, rawGamepad),
							_1: _p49
						},
						_1: _p50
					};
				}
			});
		return A3(
			_elm_lang$core$List$foldr,
			foldRawGamepad,
			{
				ctor: '_Tuple2',
				_0: {ctor: '[]'},
				_1: {ctor: '[]'}
			},
			_xarvh$elm_gamepad$Gamepad$getRawGamepads(blob));
	});
var _xarvh$elm_gamepad$Gamepad$getGamepads = F2(
	function (database, blob) {
		return _elm_lang$core$Tuple$first(
			A2(_xarvh$elm_gamepad$Gamepad$getKnownAndUnknownGamepads, database, blob));
	});
var _xarvh$elm_gamepad$Gamepad$getUnknownGamepads = F2(
	function (database, blob) {
		return _elm_lang$core$Tuple$second(
			A2(_xarvh$elm_gamepad$Gamepad$getKnownAndUnknownGamepads, database, blob));
	});
var _xarvh$elm_gamepad$Gamepad$aIsPressed = _xarvh$elm_gamepad$Gamepad$isPressed(_xarvh$elm_gamepad$Gamepad$A);

var _xarvh$elm_gamepad$Gamepad_Remap$getCurrentButton = function (unionModel) {
	var _p0 = unionModel;
	if (_p0.ctor === 'Ready') {
		return A2(
			_elm_lang$core$Maybe$map,
			_elm_lang$core$Tuple$second,
			_elm_lang$core$List$head(_p0._0.unconfiguredButtons));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _xarvh$elm_gamepad$Gamepad_Remap$view = function (model) {
	return A2(
		_elm_lang$core$Maybe$withDefault,
		'',
		_xarvh$elm_gamepad$Gamepad_Remap$getCurrentButton(model));
};
var _xarvh$elm_gamepad$Gamepad_Remap$getTargetGamepadIndex = function (unionModel) {
	var _p1 = unionModel;
	if (_p1.ctor === 'Ready') {
		return _xarvh$elm_gamepad$Gamepad$unknownGetIndex(_p1._0.targetUnknownGamepad);
	} else {
		return _p1._0;
	}
};
var _xarvh$elm_gamepad$Gamepad_Remap$indexToUnknownGamepad = F2(
	function (blob, index) {
		var isTargetGamepad = function (unknownGamepad) {
			return _elm_lang$core$Native_Utils.eq(
				_xarvh$elm_gamepad$Gamepad$unknownGetIndex(unknownGamepad),
				index);
		};
		return _elm_lang$core$List$head(
			A2(
				_elm_lang$core$List$filter,
				isTargetGamepad,
				_xarvh$elm_gamepad$Gamepad$getAllGamepadsAsUnknown(blob)));
	});
var _xarvh$elm_gamepad$Gamepad_Remap$ConfiguredEntry = F2(
	function (a, b) {
		return {destination: a, origin: b};
	});
var _xarvh$elm_gamepad$Gamepad_Remap$ModelRecord = F4(
	function (a, b, c, d) {
		return {configuredButtons: a, inputState: b, targetUnknownGamepad: c, unconfiguredButtons: d};
	});
var _xarvh$elm_gamepad$Gamepad_Remap$UpdateDatabase = function (a) {
	return {ctor: 'UpdateDatabase', _0: a};
};
var _xarvh$elm_gamepad$Gamepad_Remap$configuredButtonsToOutcome = F2(
	function (targetUnknownGamepad, configuredButtons) {
		var configuredButtonToTuple = function (button) {
			return {ctor: '_Tuple2', _0: button.destination, _1: button.origin};
		};
		var map = A2(_elm_lang$core$List$map, configuredButtonToTuple, configuredButtons);
		return _xarvh$elm_gamepad$Gamepad_Remap$UpdateDatabase(
			A2(_xarvh$elm_gamepad$Gamepad$buttonMapToUpdateDatabase, targetUnknownGamepad, map));
	});
var _xarvh$elm_gamepad$Gamepad_Remap$Error = function (a) {
	return {ctor: 'Error', _0: a};
};
var _xarvh$elm_gamepad$Gamepad_Remap$notConnectedError = function (gamepadIndex) {
	return _xarvh$elm_gamepad$Gamepad_Remap$Error(
		A2(
			_elm_lang$core$Basics_ops['++'],
			'Gamepad ',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$Basics$toString(gamepadIndex),
				' is not connected')));
};
var _xarvh$elm_gamepad$Gamepad_Remap$StillOpen = function (a) {
	return {ctor: 'StillOpen', _0: a};
};
var _xarvh$elm_gamepad$Gamepad_Remap$WaitingForAnyButtonDown = {ctor: 'WaitingForAnyButtonDown'};
var _xarvh$elm_gamepad$Gamepad_Remap$WaitingForAllButtonsUp = {ctor: 'WaitingForAllButtonsUp'};
var _xarvh$elm_gamepad$Gamepad_Remap$WaitingForGamepad = F2(
	function (a, b) {
		return {ctor: 'WaitingForGamepad', _0: a, _1: b};
	});
var _xarvh$elm_gamepad$Gamepad_Remap$init = F2(
	function (gamepadIndex, buttonsToConfigure) {
		return A2(_xarvh$elm_gamepad$Gamepad_Remap$WaitingForGamepad, gamepadIndex, buttonsToConfigure);
	});
var _xarvh$elm_gamepad$Gamepad_Remap$Ready = function (a) {
	return {ctor: 'Ready', _0: a};
};
var _xarvh$elm_gamepad$Gamepad_Remap$actuallyInit = F3(
	function (blob, gamepadIndex, buttonsToConfigure) {
		var _p2 = A2(_xarvh$elm_gamepad$Gamepad_Remap$indexToUnknownGamepad, blob, gamepadIndex);
		if (_p2.ctor === 'Nothing') {
			return _xarvh$elm_gamepad$Gamepad_Remap$notConnectedError(gamepadIndex);
		} else {
			return _xarvh$elm_gamepad$Gamepad_Remap$StillOpen(
				_xarvh$elm_gamepad$Gamepad_Remap$Ready(
					{
						configuredButtons: {ctor: '[]'},
						unconfiguredButtons: buttonsToConfigure,
						inputState: _xarvh$elm_gamepad$Gamepad_Remap$WaitingForAllButtonsUp,
						targetUnknownGamepad: _p2._0
					}));
		}
	});
var _xarvh$elm_gamepad$Gamepad_Remap$skipCurrentButton = function (unionModel) {
	var _p3 = unionModel;
	if (_p3.ctor === 'WaitingForGamepad') {
		return _xarvh$elm_gamepad$Gamepad_Remap$StillOpen(unionModel);
	} else {
		var _p6 = _p3._0;
		var _p4 = _p6.unconfiguredButtons;
		if (_p4.ctor === '[]') {
			return A2(_xarvh$elm_gamepad$Gamepad_Remap$configuredButtonsToOutcome, _p6.targetUnknownGamepad, _p6.configuredButtons);
		} else {
			var _p5 = _p4._1;
			return _elm_lang$core$Native_Utils.eq(
				_p5,
				{ctor: '[]'}) ? A2(_xarvh$elm_gamepad$Gamepad_Remap$configuredButtonsToOutcome, _p6.targetUnknownGamepad, _p6.configuredButtons) : _xarvh$elm_gamepad$Gamepad_Remap$StillOpen(
				_xarvh$elm_gamepad$Gamepad_Remap$Ready(
					_elm_lang$core$Native_Utils.update(
						_p6,
						{unconfiguredButtons: _p5, inputState: _xarvh$elm_gamepad$Gamepad_Remap$WaitingForAllButtonsUp})));
		}
	}
};
var _xarvh$elm_gamepad$Gamepad_Remap$onButtonPress = F2(
	function (origin, model) {
		var _p7 = model.unconfiguredButtons;
		if (_p7.ctor === '[]') {
			return A2(_xarvh$elm_gamepad$Gamepad_Remap$configuredButtonsToOutcome, model.targetUnknownGamepad, model.configuredButtons);
		} else {
			var _p8 = _p7._1;
			var buttonConfig = {
				destination: _elm_lang$core$Tuple$first(_p7._0),
				origin: origin
			};
			var configuredButtons = {ctor: '::', _0: buttonConfig, _1: model.configuredButtons};
			return _elm_lang$core$Native_Utils.eq(
				_p8,
				{ctor: '[]'}) ? A2(_xarvh$elm_gamepad$Gamepad_Remap$configuredButtonsToOutcome, model.targetUnknownGamepad, configuredButtons) : _xarvh$elm_gamepad$Gamepad_Remap$StillOpen(
				_xarvh$elm_gamepad$Gamepad_Remap$Ready(
					_elm_lang$core$Native_Utils.update(
						model,
						{configuredButtons: configuredButtons, unconfiguredButtons: _p8})));
		}
	});
var _xarvh$elm_gamepad$Gamepad_Remap$onMaybePressedButton = F2(
	function (maybeOrigin, model) {
		var _p9 = {ctor: '_Tuple2', _0: model.inputState, _1: maybeOrigin};
		if (_p9._0.ctor === 'WaitingForAllButtonsUp') {
			if (_p9._1.ctor === 'Just') {
				return _xarvh$elm_gamepad$Gamepad_Remap$StillOpen(
					_xarvh$elm_gamepad$Gamepad_Remap$Ready(model));
			} else {
				return _xarvh$elm_gamepad$Gamepad_Remap$StillOpen(
					_xarvh$elm_gamepad$Gamepad_Remap$Ready(
						_elm_lang$core$Native_Utils.update(
							model,
							{inputState: _xarvh$elm_gamepad$Gamepad_Remap$WaitingForAnyButtonDown})));
			}
		} else {
			if (_p9._1.ctor === 'Just') {
				return A2(
					_xarvh$elm_gamepad$Gamepad_Remap$onButtonPress,
					_p9._1._0,
					_elm_lang$core$Native_Utils.update(
						model,
						{inputState: _xarvh$elm_gamepad$Gamepad_Remap$WaitingForAllButtonsUp}));
			} else {
				return _xarvh$elm_gamepad$Gamepad_Remap$StillOpen(
					_xarvh$elm_gamepad$Gamepad_Remap$Ready(model));
			}
		}
	});
var _xarvh$elm_gamepad$Gamepad_Remap$update = F2(
	function (msg, unionModel) {
		var _p10 = msg;
		var _p15 = _p10._0._1;
		var _p11 = unionModel;
		if (_p11.ctor === 'WaitingForGamepad') {
			return A3(_xarvh$elm_gamepad$Gamepad_Remap$actuallyInit, _p15, _p11._0, _p11._1);
		} else {
			var _p14 = _p11._0;
			var _p12 = A2(
				_xarvh$elm_gamepad$Gamepad_Remap$indexToUnknownGamepad,
				_p15,
				_xarvh$elm_gamepad$Gamepad$unknownGetIndex(_p14.targetUnknownGamepad));
			if (_p12.ctor === 'Nothing') {
				return _xarvh$elm_gamepad$Gamepad_Remap$notConnectedError(
					_xarvh$elm_gamepad$Gamepad$unknownGetIndex(_p14.targetUnknownGamepad));
			} else {
				var _p13 = _p12._0;
				return A2(
					_xarvh$elm_gamepad$Gamepad_Remap$onMaybePressedButton,
					_xarvh$elm_gamepad$Gamepad$estimateOrigin(_p13),
					_elm_lang$core$Native_Utils.update(
						_p14,
						{targetUnknownGamepad: _p13}));
			}
		}
	});
var _xarvh$elm_gamepad$Gamepad_Remap$OnGamepad = function (a) {
	return {ctor: 'OnGamepad', _0: a};
};
var _xarvh$elm_gamepad$Gamepad_Remap$testMsg = function (blob) {
	return _xarvh$elm_gamepad$Gamepad_Remap$OnGamepad(
		{ctor: '_Tuple2', _0: 16.6, _1: blob});
};
var _xarvh$elm_gamepad$Gamepad_Remap$subscriptions = function (portSubscription) {
	return portSubscription(_xarvh$elm_gamepad$Gamepad_Remap$OnGamepad);
};

var _xarvh$elm_haifisch$ColorPattern$neutral = {bright: '#bbb', dark: '#999', key: 'grey'};
var _xarvh$elm_haifisch$ColorPattern$get = F2(
	function (index, colorPatterns) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			_xarvh$elm_haifisch$ColorPattern$neutral,
			A2(
				_elm_lang$core$Array$get,
				A2(
					_elm_lang$core$Basics_ops['%'],
					index,
					_elm_lang$core$Array$length(colorPatterns)),
				colorPatterns));
	});
var _xarvh$elm_haifisch$ColorPattern$patterns = _elm_lang$core$Array$fromList(
	{
		ctor: '::',
		_0: {bright: '#f00', dark: '#900', key: 'red'},
		_1: {
			ctor: '::',
			_0: {bright: '#0f0', dark: '#090', key: 'green'},
			_1: {
				ctor: '::',
				_0: {bright: '#00f', dark: '#009', key: 'blue'},
				_1: {
					ctor: '::',
					_0: {bright: '#0ee', dark: '#0bb', key: 'cyan'},
					_1: {
						ctor: '::',
						_0: {bright: '#f0f', dark: '#909', key: 'purple'},
						_1: {
							ctor: '::',
							_0: {bright: '#ee0', dark: '#bb0', key: 'yellow'},
							_1: {ctor: '[]'}
						}
					}
				}
			}
		}
	});
var _xarvh$elm_haifisch$ColorPattern$ColorPattern = F3(
	function (a, b, c) {
		return {bright: a, dark: b, key: c};
	});

var _xarvh$elm_haifisch$Common$rotateVector = F2(
	function (angle, v) {
		var cosA = _elm_lang$core$Basics$cos(angle);
		var sinA = _elm_lang$core$Basics$sin(angle);
		var _p0 = _elm_community$linear_algebra$Math_Vector2$toTuple(v);
		var x = _p0._0;
		var y = _p0._1;
		return A2(_elm_community$linear_algebra$Math_Vector2$vec2, (x * cosA) - (y * sinA), (x * sinA) + (y * cosA));
	});
var _xarvh$elm_haifisch$Common$rightHandNormal = function (v) {
	var _p1 = _elm_community$linear_algebra$Math_Vector2$toTuple(v);
	var x = _p1._0;
	var y = _p1._1;
	return A2(_elm_community$linear_algebra$Math_Vector2$vec2, 0 - y, x);
};
var _xarvh$elm_haifisch$Common$normalizeAngle = function (a) {
	return (_elm_lang$core$Native_Utils.cmp(a, 0 - _elm_lang$core$Basics$pi) < 0) ? (a + (2 * _elm_lang$core$Basics$pi)) : ((_elm_lang$core$Native_Utils.cmp(a, _elm_lang$core$Basics$pi) > 0) ? (a - (2 * _elm_lang$core$Basics$pi)) : a);
};
var _xarvh$elm_haifisch$Common$angleToVector = function (a) {
	return A2(
		_elm_community$linear_algebra$Math_Vector2$vec2,
		_elm_lang$core$Basics$cos(a),
		_elm_lang$core$Basics$sin(a));
};
var _xarvh$elm_haifisch$Common$vectorToAngle = function (v) {
	var _p2 = _elm_community$linear_algebra$Math_Vector2$toTuple(v);
	var x = _p2._0;
	var y = _p2._1;
	return A2(_elm_lang$core$Basics$atan2, y, x);
};
var _xarvh$elm_haifisch$Common$clampToRadius = F2(
	function (radius, v) {
		var ll = _elm_community$linear_algebra$Math_Vector2$lengthSquared(v);
		return (_elm_lang$core$Native_Utils.cmp(ll, radius * radius) < 1) ? v : A2(
			_elm_community$linear_algebra$Math_Vector2$scale,
			radius / _elm_lang$core$Basics$sqrt(ll),
			v);
	});
var _xarvh$elm_haifisch$Common$vectorToString = function (v) {
	return A2(
		_elm_lang$core$Basics_ops['++'],
		_elm_lang$core$Basics$toString(
			_elm_community$linear_algebra$Math_Vector2$getX(v)),
		A2(
			_elm_lang$core$Basics_ops['++'],
			',',
			_elm_lang$core$Basics$toString(
				_elm_community$linear_algebra$Math_Vector2$getY(v))));
};
var _xarvh$elm_haifisch$Common$v0 = A2(_elm_community$linear_algebra$Math_Vector2$vec2, 0, 0);
var _xarvh$elm_haifisch$Common$shipLength = 1.0;
var _xarvh$elm_haifisch$Common$worldRadius = 17 * _xarvh$elm_haifisch$Common$shipLength;
var _xarvh$elm_haifisch$Common$Player = F4(
	function (a, b, c, d) {
		return {id: a, isConnected: b, score: c, colorPattern: d};
	});
var _xarvh$elm_haifisch$Common$InputState = F3(
	function (a, b, c) {
		return {finalAim: a, fire: b, move: c};
	});
var _xarvh$elm_haifisch$Common$Ship = F9(
	function (a, b, c, d, e, f, g, h, i) {
		return {id: a, playerId: b, position: c, heading: d, status: e, name: f, reloadTime: g, explodeTime: h, respawnTime: i};
	});
var _xarvh$elm_haifisch$Common$Projectile = F3(
	function (a, b, c) {
		return {playerId: a, position: b, heading: c};
	});
var _xarvh$elm_haifisch$Common$Planet = F5(
	function (a, b, c, d, e) {
		return {orbitRadius: a, angularSpeed: b, angle: c, surfaceRadius: d, satellites: e};
	});
var _xarvh$elm_haifisch$Common$Satellite = F3(
	function (a, b, c) {
		return {orbitRadius: a, angularSpeed: b, angle: c};
	});
var _xarvh$elm_haifisch$Common$RemoveShip = function (a) {
	return {ctor: 'RemoveShip', _0: a};
};
var _xarvh$elm_haifisch$Common$DamageShip = function (a) {
	return {ctor: 'DamageShip', _0: a};
};
var _xarvh$elm_haifisch$Common$RemoveProjectile = function (a) {
	return {ctor: 'RemoveProjectile', _0: a};
};
var _xarvh$elm_haifisch$Common$CreateShip = function (a) {
	return {ctor: 'CreateShip', _0: a};
};
var _xarvh$elm_haifisch$Common$AddProjectile = function (a) {
	return {ctor: 'AddProjectile', _0: a};
};
var _xarvh$elm_haifisch$Common$ShipDamagesShip = F2(
	function (a, b) {
		return {ctor: 'ShipDamagesShip', _0: a, _1: b};
	});
var _xarvh$elm_haifisch$Common$ShipActivates = function (a) {
	return {ctor: 'ShipActivates', _0: a};
};
var _xarvh$elm_haifisch$Common$ShipAppears = function (a) {
	return {ctor: 'ShipAppears', _0: a};
};
var _xarvh$elm_haifisch$Common$ShipFires = function (a) {
	return {ctor: 'ShipFires', _0: a};
};
var _xarvh$elm_haifisch$Common$ShipExplodes = function (a) {
	return {ctor: 'ShipExplodes', _0: a};
};
var _xarvh$elm_haifisch$Common$E = function (a) {
	return {ctor: 'E', _0: a};
};
var _xarvh$elm_haifisch$Common$D = function (a) {
	return {ctor: 'D', _0: a};
};
var _xarvh$elm_haifisch$Common$Exploding = {ctor: 'Exploding'};
var _xarvh$elm_haifisch$Common$Active = {ctor: 'Active'};
var _xarvh$elm_haifisch$Common$Spawning = {ctor: 'Spawning'};

var _xarvh$elm_haifisch$Ship$transform = F2(
	function (ship, polygon) {
		return A2(
			_elm_lang$core$List$map,
			function (_p0) {
				return A2(
					_elm_community$linear_algebra$Math_Vector2$add,
					ship.position,
					A2(_xarvh$elm_haifisch$Common$rotateVector, ship.heading, _p0));
			},
			polygon);
	});
var _xarvh$elm_haifisch$Ship$spawnDuration = 3 * _elm_lang$core$Time$second;
var _xarvh$elm_haifisch$Ship$explosionDuration = 1000 * _elm_lang$core$Time$millisecond;
var _xarvh$elm_haifisch$Ship$reloadTime = 0.3 * _elm_lang$core$Time$second;
var _xarvh$elm_haifisch$Ship$turningRate = _elm_lang$core$Basics$turns(1) / _elm_lang$core$Time$second;
var _xarvh$elm_haifisch$Ship$length = _xarvh$elm_haifisch$Common$shipLength;
var _xarvh$elm_haifisch$Ship$speed = (7 * _xarvh$elm_haifisch$Ship$length) / _elm_lang$core$Time$second;
var _xarvh$elm_haifisch$Ship$mesh = A2(
	_elm_lang$core$List$map,
	function (_p1) {
		return A2(
			_elm_community$linear_algebra$Math_Vector2$scale,
			_xarvh$elm_haifisch$Ship$length / 15,
			_elm_community$linear_algebra$Math_Vector2$fromTuple(_p1));
	},
	{
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: -3, _1: 0},
		_1: {
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: -5, _1: 5},
			_1: {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 10, _1: 0},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: -5, _1: -5},
					_1: {ctor: '[]'}
				}
			}
		}
	});
var _xarvh$elm_haifisch$Ship$convexMesh = A2(_elm_lang$core$List$drop, 1, _xarvh$elm_haifisch$Ship$mesh);
var _xarvh$elm_haifisch$Ship$radius = A2(
	_elm_lang$core$Maybe$withDefault,
	0,
	_elm_lang$core$List$maximum(
		A2(_elm_lang$core$List$map, _elm_community$linear_algebra$Math_Vector2$length, _xarvh$elm_haifisch$Ship$convexMesh)));

var _xarvh$elm_haifisch$Collision$normalIsSeparatingAxis = F2(
	function (q, _p0) {
		var _p1 = _p0;
		var _p2 = _p1._0;
		var n = _xarvh$elm_haifisch$Common$rightHandNormal(
			A2(_elm_community$linear_algebra$Math_Vector2$sub, _p1._1, _p2));
		var isRightSide = function (p) {
			return _elm_lang$core$Native_Utils.cmp(
				A2(
					_elm_community$linear_algebra$Math_Vector2$dot,
					n,
					A2(_elm_community$linear_algebra$Math_Vector2$sub, p, _p2)),
				0) > 0;
		};
		return A2(_elm_lang$core$List$all, isRightSide, q);
	});
var _xarvh$elm_haifisch$Collision$anySegment = F2(
	function (f, poly) {
		var a = _elm_lang$core$Array$fromList(poly);
		var get = function (index) {
			return A2(
				_elm_lang$core$Maybe$withDefault,
				A2(_elm_community$linear_algebra$Math_Vector2$vec2, 0, 0),
				A2(
					_elm_lang$core$Array$get,
					A2(
						_elm_lang$core$Basics_ops['%'],
						index,
						_elm_lang$core$Array$length(a)),
					a));
		};
		var segments = A2(
			_elm_lang$core$List$indexedMap,
			F2(
				function (index, v) {
					return {
						ctor: '_Tuple2',
						_0: get(index),
						_1: get(index + 1)
					};
				}),
			poly);
		return A2(_elm_lang$core$List$any, f, segments);
	});
var _xarvh$elm_haifisch$Collision$halfCollision = F2(
	function (p, q) {
		return !A2(
			_xarvh$elm_haifisch$Collision$anySegment,
			_xarvh$elm_haifisch$Collision$normalIsSeparatingAxis(q),
			p);
	});
var _xarvh$elm_haifisch$Collision$collisionPolygonVsPolygon = F2(
	function (p, q) {
		return A2(_xarvh$elm_haifisch$Collision$halfCollision, p, q) && A2(_xarvh$elm_haifisch$Collision$halfCollision, q, p);
	});
var _xarvh$elm_haifisch$Collision$projectileVsShip = F3(
	function (a, b, ship) {
		var _p3 = _elm_community$linear_algebra$Math_Vector2$toTuple(
			A2(_elm_community$linear_algebra$Math_Vector2$sub, a, b));
		var dx = _p3._0;
		var dy = _p3._1;
		var radius = A2(
			_elm_lang$core$Basics$max,
			_elm_lang$core$Basics$abs(dx),
			_elm_lang$core$Basics$abs(dy));
		var minimumCollisionDistance = radius + _xarvh$elm_haifisch$Ship$radius;
		return (_elm_lang$core$Native_Utils.cmp(
			A2(_elm_community$linear_algebra$Math_Vector2$distanceSquared, a, ship.position),
			minimumCollisionDistance * minimumCollisionDistance) > 0) ? false : A2(
			_xarvh$elm_haifisch$Collision$collisionPolygonVsPolygon,
			{
				ctor: '::',
				_0: a,
				_1: {
					ctor: '::',
					_0: b,
					_1: {ctor: '[]'}
				}
			},
			A2(_xarvh$elm_haifisch$Ship$transform, ship, _xarvh$elm_haifisch$Ship$convexMesh));
	});

var _xarvh$lexical_random_generator$LexicalRandom$choices = F2(
	function ($default, array) {
		return A2(
			_elm_lang$core$Random$andThen,
			_elm_lang$core$Maybe$withDefault($default),
			_elm_community$random_extra$Random_Array$sample(array));
	});
var _xarvh$lexical_random_generator$LexicalRandom$generator = F3(
	function (filler, lexicon, key) {
		var _p0 = A2(_elm_lang$core$Dict$get, key, lexicon);
		if (_p0.ctor === 'Nothing') {
			return _elm_community$random_extra$Random_Extra$constant(filler);
		} else {
			var reducedLexicon = A2(_elm_lang$core$Dict$remove, key, lexicon);
			var fragmentToGenerator = function (fragment) {
				var _p1 = fragment;
				if (_p1.ctor === 'Key') {
					return A3(_xarvh$lexical_random_generator$LexicalRandom$generator, filler, reducedLexicon, _p1._0);
				} else {
					return _elm_community$random_extra$Random_Extra$constant(_p1._0);
				}
			};
			var definitionToGenerator = function (definition) {
				return A2(
					_elm_lang$core$Random$map,
					_elm_lang$core$String$join(''),
					_elm_community$random_extra$Random_Extra$combine(
						A2(_elm_lang$core$List$map, fragmentToGenerator, definition)));
			};
			return A2(
				_xarvh$lexical_random_generator$LexicalRandom$choices,
				_elm_community$random_extra$Random_Extra$constant(''),
				A2(_elm_lang$core$Array$map, definitionToGenerator, _p0._0));
		}
	});
var _xarvh$lexical_random_generator$LexicalRandom$Key = function (a) {
	return {ctor: 'Key', _0: a};
};
var _xarvh$lexical_random_generator$LexicalRandom$Constant = function (a) {
	return {ctor: 'Constant', _0: a};
};
var _xarvh$lexical_random_generator$LexicalRandom$fromString = function (stringLexicon) {
	var stringToDefinition = function (stringDefinition) {
		var chunkToFrags = function (chunk) {
			var _p2 = A2(_elm_lang$core$String$split, '}', chunk);
			if (((_p2.ctor === '::') && (_p2._1.ctor === '::')) && (_p2._1._1.ctor === '[]')) {
				return {
					ctor: '::',
					_0: _xarvh$lexical_random_generator$LexicalRandom$Key(_p2._0),
					_1: {
						ctor: '::',
						_0: _xarvh$lexical_random_generator$LexicalRandom$Constant(_p2._1._0),
						_1: {ctor: '[]'}
					}
				};
			} else {
				return {
					ctor: '::',
					_0: _xarvh$lexical_random_generator$LexicalRandom$Constant(chunk),
					_1: {ctor: '[]'}
				};
			}
		};
		return _elm_lang$core$List$concat(
			A2(
				_elm_lang$core$List$map,
				chunkToFrags,
				A2(_elm_lang$core$String$split, '{', stringDefinition)));
	};
	var addToLexiconKey = F3(
		function (key, line, lexicon) {
			var existingDefinitions = A2(
				_elm_lang$core$Maybe$withDefault,
				_elm_lang$core$Array$empty,
				A2(_elm_lang$core$Dict$get, key, lexicon));
			var newDefinitions = _elm_lang$core$Array$fromList(
				A2(
					_elm_lang$core$List$map,
					stringToDefinition,
					A2(
						_elm_lang$core$List$filter,
						F2(
							function (x, y) {
								return !_elm_lang$core$Native_Utils.eq(x, y);
							})(''),
						A2(
							_elm_lang$core$List$map,
							_elm_lang$core$String$trim,
							A2(_elm_lang$core$String$split, ',', line)))));
			return A3(
				_elm_lang$core$Dict$insert,
				key,
				A2(_elm_lang$core$Array$append, existingDefinitions, newDefinitions),
				lexicon);
		});
	var addLine = F2(
		function (line, _p3) {
			var _p4 = _p3;
			var _p6 = _p4._1;
			var _p5 = _p4._0;
			return A2(
				_elm_lang$core$Regex$contains,
				_elm_lang$core$Regex$regex('^\\s*#'),
				line) ? {ctor: '_Tuple2', _0: _p5, _1: _p6} : (A2(
				_elm_lang$core$Regex$contains,
				_elm_lang$core$Regex$regex('^\\s'),
				line) ? {
				ctor: '_Tuple2',
				_0: _p5,
				_1: A3(addToLexiconKey, _p5, line, _p6)
			} : {ctor: '_Tuple2', _0: line, _1: _p6});
		});
	return _elm_lang$core$Tuple$second(
		A3(
			_elm_lang$core$List$foldl,
			addLine,
			{ctor: '_Tuple2', _0: 'default', _1: _elm_lang$core$Dict$empty},
			_elm_lang$core$String$lines(stringLexicon)));
};

var _xarvh$elm_haifisch$Names$defaultLexicon = _xarvh$lexical_random_generator$LexicalRandom$fromString('\n\n# These phonems have been generated from a dictionary of common english words\n# not ideal (I wager a dictionary of proper nouns only would yield better results)\n# but for now will do.\n\nini\n    in,re,un,con,de,dis,ex,im,an,com,en,al,pro,pre,per,over,as,ar,di,mis,be,ac,sub,ad,ma,mar,car,out,ap,au,or,for,ob,\n    par,co,se,em,man,vi,non,am,mo,su,ab,cor,ca,pa,es,hy,can,bar,mi,col,so,mon,at,up,ir,ver,ra,mer,lu,gen,trans,pe,ro,\n    po,op,ter,sen,fore,coun,pen,el,cal,af,no,fa,bi,mag,hu,min,mu,mul,back,har,ju,eu,tran,ten,pi,gar,ed,cat,mil,li,sun,\n    lo,pan,hand,fer,sur,her,elec,la,os,mal,ef,ag,fi,ban,to,pu,mor,wa,cu,cir,me,nu,ge,tri,tor,du,cen,sa,home,head,va,mas,\n    il,mid,er,wood,win,oc,mod,cur,syn,na,pur,hon,of,sup,bur,uni,do,hi,lin,pri,psy,the,pas,pho,black,char,cre,pal,ho,\n    mc,cas,sym,ser,air,ru,san,ne,men,down,he,tu,tem,sta,fu,fire,ba,sim,tan,por,sec,is,cap,para,cam,poly,work,post,sul,\n    rec,hor,des,ve,le,hol,fal,cru,res,house,short,sal,gal\n\nmid\n    i,ti,a,o,er,ter,u,ri,to,si,cal,di,ca,al,ta,li,ni,tion,per,der,ra,tu,e,ful,na,ma,la,ing,fi,sa,ci,ous,con,is,en,re,\n    mi,men,bil,less,cu,tri,ten,te,tive,pi,mo,po,pa,sive,ber,bi,ish,com,man,gi,de,tal,vi,ga,va,in,ro,tra,ed,an,pe,ver,\n    so,den,pen,pos,tro,mu,pli,wom,mat,or,le,son,lar,lu,no,nal,sion,fer,co,gen,for,cious,pro,log,cep,lo,ac,tis,cen,ar,\n    pre,sen,hous,ces,mer,mis,da,el,ol,se,tar,fec,sis,ge,duc,su,por,ven,dis,tur,do,pres,ger,struc,jec,tan,ne,as,tle,\n    me,go,tain,ba,mon,ex,tic,cil,tor,par,il,tiv,graph,cial,sti,cel,tial,gan,cis,ton,gu,cer,be,nis,car,y,low,cum,at,\n    thet,sur,til,ser,abil,wor,nu,tin,tious,ble,fa,cus,bu,nar,mar,mal,met,rec,ple,pu,alis,lec,ap,nom,the,sid,vis,\n    ness,cul,pri,cur,hen,cre,dic,ner,mor,tol,can,tel,ui,fu,cy,trac,lous\n\nend\n    ly,es,ness,er,est,ers,tions,ty,tion,able,ic,ings,ments,ry,ties,tors,al,cal,man,ters,less,cy,ous,tive,ful,men,\n    ates,ble,an,tic,ists,gy,na,ies,sions,son,ans,ta,ment,ton,ism,ries,ics,bles,bly,als,fies,fy,la,da,en,lates,\n    tives,ders,ny,ably,tures,cies,pers,sive,ish,ter,cates,ley,tal,sis,dles,gles,ery,by,to,gists,ens,nates,nia,\n    tles,ma,cles,lies,tics,um,my,ways,do,land,ra,sion,ca,las,cious,gers,gies,lar,ages,bers,nal,ets,gates,ums,\n    ship,els,boards,tates,les,tial,heads,mas,sy,py,den,ples,der,dy,lines,ka,tos,tious,nas,lo,ary,co,rates,phy,\n    tary,some,mates,ti,ships,lets,nies,its,lands,try,va,ses,cent,ni,tis,rooms,nous,sors,work,backs,lars,line,\n    stones,fuls,dents,tine,outs,tals,wood,wards,tons,ances,ners,cals,sia,ba,cians,ors,go,thy,sides,ki,lous,di,\n    mers,ia,cence,ent,phers,dies,most,ga,sons,ons,downs,mi,sters,lows,dos,on,balls,lent,tus,ate,grams,ten,ables,\n    ture,dence,los,ford,bies,nals,holes,nary,sa,sures\n\nproperNoun\n    {ini}{end}\n    {ini}{mid}{end}\n\nadverb\n    always,inevitably,necessarily,surely,inescapably,assuredly\n\nsuperlativeAdjective\n    flawless,victorious,favoured,triumphant,successful,fortunate,lucky,outstanding,strong\n    auspicious,crowned,extraordinary,unbeaten,undefeated,unconquered,prevailing,excellent,superior,greatest\n    illustrious,splendid,fierce\n    amazing,awesome,excellent,fabulous,fantastic,favorable,fortuitous,ineffable,perfect,propitious,spectacular,wondrous\n\ngenericAdjective\n    mighty,great,straight,lightning\n\nnoun\n    champion,challenger,defender,conqueror,guardian,paladin,vanquisher,victor,warrior,augury\n    hammer,mallet,anvil\n    sword,mercy\n    blade,sabre,dagger,scimitar,foil,glaive\n    arrow\n    fury,anger,wrath,storm,lightning,thunder,omen,vengeance\n    light,sunrise,peace\n    Sun,Moon,Daystar\n    cross,\n\n\npotentiallyQualifiedNoun\n    {noun}\n    {noun}\n    {noun} of {properNoun}\n    {noun} of {properNoun}\n    {noun} of the Gods\n\nexalted\n    {adverb} {superlativeAdjective}\n    {adverb} {superlativeAdjective} {potentiallyQualifiedNoun}\n    {superlativeAdjective} {potentiallyQualifiedNoun}\n\nship\n    {noun}\n    {genericAdjective} {properNoun}\n    {properNoun}\n    {color} {properNoun}\n    {color} {properNoun}\n    {genericAdjective} {noun}\n    {color} {noun}\n    {color} {noun}\n    {superlativeAdjective}\n    {exalted}\n\nfleet\n    {color} Fleet\n    {properNoun} Fleet\n    {noun} Fleet\n\nred\n    red,ruby,crimson,vermillion,fiery,sanguine,burning,searing,\n\ngreen\n    green,verdant,emerald,jade,lime,viridian\n\nblue\n    blue,sapphire,cerulean,cobalt,ultramarine\n\ncyan\n    azure,cerulean,sky,pale\n\npurple\n    purple,lilac,violet,amethyst\n\nyellow\n    yellow,amber,chrome,ivory,gold,golden,saffron,sand\n');
var _xarvh$elm_haifisch$Names$ship = function (colorName) {
	var fragment = _xarvh$lexical_random_generator$LexicalRandom$Key(colorName);
	var definition = {
		ctor: '::',
		_0: fragment,
		_1: {ctor: '[]'}
	};
	var entry = _elm_lang$core$Array$fromList(
		{
			ctor: '::',
			_0: definition,
			_1: {ctor: '[]'}
		});
	var lexicon = A3(_elm_lang$core$Dict$insert, 'color', entry, _xarvh$elm_haifisch$Names$defaultLexicon);
	return A2(
		_elm_lang$core$Random$map,
		_elm_community$string_extra$String_Extra$toTitleCase,
		A3(_xarvh$lexical_random_generator$LexicalRandom$generator, '---', lexicon, 'ship'));
};

var _xarvh$elm_haifisch$Planet$angularSpeed = function (orbitRadius) {
	return ((1.0e-2 / _elm_lang$core$Time$second) * _elm_lang$core$Basics$turns(1)) * Math.pow(orbitRadius / _xarvh$elm_haifisch$Common$worldRadius, -1.5);
};
var _xarvh$elm_haifisch$Planet$wrS = _elm_lang$core$Random$map(
	F2(
		function (x, y) {
			return x * y;
		})(_xarvh$elm_haifisch$Common$worldRadius / _elm_lang$core$Time$second));
var _xarvh$elm_haifisch$Planet$wr = _elm_lang$core$Random$map(
	F2(
		function (x, y) {
			return x * y;
		})(_xarvh$elm_haifisch$Common$worldRadius));
var _xarvh$elm_haifisch$Planet$randomAngle = A2(
	_elm_lang$core$Random$float,
	_elm_lang$core$Basics$turns(-0.5),
	_elm_lang$core$Basics$turns(0.5));
var _xarvh$elm_haifisch$Planet$satelliteGenerator = A4(
	_elm_lang$core$Random$map3,
	_xarvh$elm_haifisch$Common$Satellite,
	_xarvh$elm_haifisch$Planet$wr(
		A2(_elm_lang$core$Random$float, 1.0e-2, 5.0e-2)),
	_xarvh$elm_haifisch$Planet$wrS(
		A2(_elm_lang$core$Random$float, 3.0e-2, 0.15)),
	_xarvh$elm_haifisch$Planet$randomAngle);
var _xarvh$elm_haifisch$Planet$planetGenerator = A5(
	_elm_lang$core$Random$map4,
	F4(
		function (orbitRadius, surfaceRadius, angle, satellites) {
			return {
				orbitRadius: orbitRadius,
				angularSpeed: _xarvh$elm_haifisch$Planet$angularSpeed(orbitRadius),
				angle: angle,
				surfaceRadius: surfaceRadius,
				satellites: satellites
			};
		}),
	_xarvh$elm_haifisch$Planet$wr(
		A2(_elm_lang$core$Random$float, 0.1, 0.9)),
	_xarvh$elm_haifisch$Planet$wr(
		A2(_elm_lang$core$Random$float, 3.0e-3, 9.0e-3)),
	_xarvh$elm_haifisch$Planet$randomAngle,
	A2(
		_elm_lang$core$Random$andThen,
		function (numberOfSatellites) {
			return A2(_elm_lang$core$Random$list, numberOfSatellites, _xarvh$elm_haifisch$Planet$satelliteGenerator);
		},
		A2(_elm_lang$core$Random$int, 0, 4)));
var _xarvh$elm_haifisch$Planet$planetsGenerator = A2(
	_elm_lang$core$Random$andThen,
	function (numberOfPlanets) {
		return A2(_elm_lang$core$Random$list, numberOfPlanets, _xarvh$elm_haifisch$Planet$planetGenerator);
	},
	A2(_elm_lang$core$Random$int, 2, 4));

var _xarvh$elm_haifisch$Game$splitOutcomes = function (outcomes) {
	var folder = F2(
		function (outcome, _p0) {
			var _p1 = _p0;
			var _p4 = _p1._1;
			var _p3 = _p1._0;
			var _p2 = outcome;
			if (_p2.ctor === 'D') {
				return {
					ctor: '_Tuple2',
					_0: {ctor: '::', _0: _p2._0, _1: _p3},
					_1: _p4
				};
			} else {
				return {
					ctor: '_Tuple2',
					_0: _p3,
					_1: {ctor: '::', _0: _p2._0, _1: _p4}
				};
			}
		});
	return A3(
		_elm_lang$core$List$foldl,
		folder,
		{
			ctor: '_Tuple2',
			_0: {ctor: '[]'},
			_1: {ctor: '[]'}
		},
		outcomes);
};
var _xarvh$elm_haifisch$Game$updateShip = F2(
	function (ship, model) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				shipsById: A3(_elm_lang$core$Dict$insert, ship.playerId, ship, model.shipsById)
			});
	});
var _xarvh$elm_haifisch$Game$addPlayer = function (model) {
	var lastId = A2(
		_elm_lang$core$Maybe$withDefault,
		0,
		_elm_lang$core$List$maximum(
			A2(
				_elm_lang$core$List$map,
				function (_) {
					return _.id;
				},
				model.players)));
	var newId = lastId + 1;
	var player = {
		id: newId,
		isConnected: true,
		score: 0,
		colorPattern: A2(_xarvh$elm_haifisch$ColorPattern$get, newId, model.shuffledColorPatterns)
	};
	var players = {ctor: '::', _0: player, _1: model.players};
	return {
		ctor: '_Tuple2',
		_0: _elm_lang$core$Native_Utils.update(
			model,
			{players: players}),
		_1: player
	};
};
var _xarvh$elm_haifisch$Game$playerTick = F3(
	function (model, inputStateByPlayerId, player) {
		var maybeShip = A2(
			_elm_lang$core$Maybe$map,
			_elm_lang$core$Tuple$second,
			A2(
				_elm_community$dict_extra$Dict_Extra$find,
				F2(
					function (id, ship) {
						return _elm_lang$core$Native_Utils.eq(ship.playerId, player.id);
					}),
				model.shipsById));
		var isConnected = A2(_elm_lang$core$Dict$member, player.id, inputStateByPlayerId);
		var events = function () {
			var _p5 = {ctor: '_Tuple2', _0: isConnected, _1: maybeShip};
			_v2_2:
			do {
				if (_p5.ctor === '_Tuple2') {
					if (_p5._0 === false) {
						if (_p5._1.ctor === 'Just') {
							return {
								ctor: '::',
								_0: _xarvh$elm_haifisch$Common$D(
									_xarvh$elm_haifisch$Common$RemoveShip(_p5._1._0.id)),
								_1: {ctor: '[]'}
							};
						} else {
							break _v2_2;
						}
					} else {
						if (_p5._1.ctor === 'Nothing') {
							return {
								ctor: '::',
								_0: _xarvh$elm_haifisch$Common$D(
									_xarvh$elm_haifisch$Common$CreateShip(player)),
								_1: {ctor: '[]'}
							};
						} else {
							break _v2_2;
						}
					}
				} else {
					break _v2_2;
				}
			} while(false);
			return {ctor: '[]'};
		}();
		return {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Native_Utils.update(
				player,
				{isConnected: isConnected}),
			_1: events
		};
	});
var _xarvh$elm_haifisch$Game$satelliteTick = F2(
	function (dt, satellite) {
		return _elm_lang$core$Native_Utils.update(
			satellite,
			{angle: satellite.angle + (dt * satellite.angularSpeed)});
	});
var _xarvh$elm_haifisch$Game$planetTick = F2(
	function (dt, planet) {
		var newSatellites = A2(
			_elm_lang$core$List$map,
			_xarvh$elm_haifisch$Game$satelliteTick(dt),
			planet.satellites);
		var newAngle = _xarvh$elm_haifisch$Common$normalizeAngle(planet.angle + (dt * planet.angularSpeed));
		return _elm_lang$core$Native_Utils.update(
			planet,
			{angle: newAngle, satellites: newSatellites});
	});
var _xarvh$elm_haifisch$Game$shipExplodeTick = F2(
	function (dt, oldShip) {
		var newExplodeTime = oldShip.explodeTime + dt;
		return (_elm_lang$core$Native_Utils.cmp(newExplodeTime, _xarvh$elm_haifisch$Ship$explosionDuration) > -1) ? {
			ctor: '_Tuple2',
			_0: oldShip,
			_1: {
				ctor: '::',
				_0: _xarvh$elm_haifisch$Common$D(
					_xarvh$elm_haifisch$Common$RemoveShip(oldShip.id)),
				_1: {ctor: '[]'}
			}
		} : {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Native_Utils.update(
				oldShip,
				{
					explodeTime: A2(_elm_lang$core$Basics$min, _xarvh$elm_haifisch$Ship$explosionDuration, newExplodeTime)
				}),
			_1: {ctor: '[]'}
		};
	});
var _xarvh$elm_haifisch$Game$shipSpawnTick = F2(
	function (dt, oldShip) {
		var newRespawnTime = oldShip.respawnTime + dt;
		return (_elm_lang$core$Native_Utils.cmp(newRespawnTime, _xarvh$elm_haifisch$Ship$spawnDuration) > 0) ? {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Native_Utils.update(
				oldShip,
				{status: _xarvh$elm_haifisch$Common$Active, reloadTime: 0}),
			_1: {
				ctor: '::',
				_0: _xarvh$elm_haifisch$Common$E(
					_xarvh$elm_haifisch$Common$ShipActivates(oldShip.playerId)),
				_1: {ctor: '[]'}
			}
		} : {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Native_Utils.update(
				oldShip,
				{respawnTime: newRespawnTime}),
			_1: {ctor: '[]'}
		};
	});
var _xarvh$elm_haifisch$Game$makeShip = F3(
	function (playerId, position, name) {
		return {
			id: playerId,
			playerId: playerId,
			heading: _xarvh$elm_haifisch$Common$vectorToAngle(
				_elm_community$linear_algebra$Math_Vector2$negate(position)),
			position: position,
			name: name,
			status: _xarvh$elm_haifisch$Common$Spawning,
			reloadTime: 0,
			respawnTime: 0,
			explodeTime: 0
		};
	});
var _xarvh$elm_haifisch$Game$newProjectile = function (ship) {
	return {playerId: ship.playerId, position: ship.position, heading: ship.heading};
};
var _xarvh$elm_haifisch$Game$shipFireControl = F3(
	function (inputState, dt, ship) {
		var _p6 = (inputState.fire && _elm_lang$core$Native_Utils.eq(ship.reloadTime, 0)) ? {
			ctor: '_Tuple2',
			_0: _xarvh$elm_haifisch$Ship$reloadTime,
			_1: {
				ctor: '::',
				_0: _xarvh$elm_haifisch$Common$D(
					_xarvh$elm_haifisch$Common$AddProjectile(
						_xarvh$elm_haifisch$Game$newProjectile(ship))),
				_1: {
					ctor: '::',
					_0: _xarvh$elm_haifisch$Common$E(
						_xarvh$elm_haifisch$Common$ShipFires(ship.playerId)),
					_1: {ctor: '[]'}
				}
			}
		} : {
			ctor: '_Tuple2',
			_0: A2(_elm_lang$core$Basics$max, 0, ship.reloadTime - dt),
			_1: {ctor: '[]'}
		};
		var newReloadTime = _p6._0;
		var deltas = _p6._1;
		var newShip = _elm_lang$core$Native_Utils.update(
			ship,
			{reloadTime: newReloadTime});
		return {ctor: '_Tuple2', _0: newShip, _1: deltas};
	});
var _xarvh$elm_haifisch$Game$init = function (seed) {
	return {
		planets: _elm_lang$core$Tuple$first(
			A2(_elm_lang$core$Random$step, _xarvh$elm_haifisch$Planet$planetsGenerator, seed)),
		players: {ctor: '[]'},
		projectiles: {ctor: '[]'},
		seed: seed,
		shipsById: _elm_lang$core$Dict$empty,
		shuffledColorPatterns: _elm_lang$core$Tuple$first(
			A2(
				_elm_lang$core$Random$step,
				_elm_community$random_extra$Random_Array$shuffle(_xarvh$elm_haifisch$ColorPattern$patterns),
				seed))
	};
};
var _xarvh$elm_haifisch$Game$outcomesOverList = F2(
	function (f, oldList) {
		var folder = F2(
			function (oldValue, _p7) {
				var _p8 = _p7;
				var _p9 = f(oldValue);
				var newValue = _p9._0;
				var newOutcomes = _p9._1;
				return {
					ctor: '_Tuple2',
					_0: {ctor: '::', _0: newValue, _1: _p8._0},
					_1: A2(_elm_lang$core$Basics_ops['++'], _p8._1, newOutcomes)
				};
			});
		return A3(
			_elm_lang$core$List$foldl,
			folder,
			{
				ctor: '_Tuple2',
				_0: {ctor: '[]'},
				_1: {ctor: '[]'}
			},
			oldList);
	});
var _xarvh$elm_haifisch$Game$outcomesOverDict = F2(
	function (f, dict) {
		var folder = F3(
			function (key, oldValue, _p10) {
				var _p11 = _p10;
				var _p12 = f(oldValue);
				var newValue = _p12._0;
				var newOutcomes = _p12._1;
				return {
					ctor: '_Tuple2',
					_0: A3(_elm_lang$core$Dict$insert, key, newValue, _p11._0),
					_1: A2(_elm_lang$core$Basics_ops['++'], _p11._1, newOutcomes)
				};
			});
		return A3(
			_elm_lang$core$Dict$foldl,
			folder,
			{
				ctor: '_Tuple2',
				_0: _elm_lang$core$Dict$empty,
				_1: {ctor: '[]'}
			},
			dict);
	});
var _xarvh$elm_haifisch$Game_ops = _xarvh$elm_haifisch$Game_ops || {};
_xarvh$elm_haifisch$Game_ops['|>>'] = F2(
	function (_p13, f) {
		var _p14 = _p13;
		var _p15 = f(_p14._0);
		var newModel = _p15._0;
		var newEffects = _p15._1;
		return {
			ctor: '_Tuple2',
			_0: newModel,
			_1: A2(_elm_lang$core$Basics_ops['++'], _p14._1, newEffects)
		};
	});
var _xarvh$elm_haifisch$Game$headingControlThreshold = 0.3;
var _xarvh$elm_haifisch$Game$velocityControlThreshold = 0.1;
var _xarvh$elm_haifisch$Game$satelliteAngularVelocity = _elm_lang$core$Basics$turns(9.0e-2) / _elm_lang$core$Time$second;
var _xarvh$elm_haifisch$Game$planetAngularVelocity = _elm_lang$core$Basics$turns(5.0e-2) / _elm_lang$core$Time$second;
var _xarvh$elm_haifisch$Game$worldRadius = _xarvh$elm_haifisch$Common$worldRadius;
var _xarvh$elm_haifisch$Game$projectileSpeed = (1 * _xarvh$elm_haifisch$Game$worldRadius) / _elm_lang$core$Time$second;
var _xarvh$elm_haifisch$Game$shipMovementControl = F3(
	function (inputState, dt, ship) {
		var maxTurn = _xarvh$elm_haifisch$Ship$turningRate * dt;
		var ignoreHeadingControl = _elm_lang$core$Native_Utils.cmp(
			_elm_community$linear_algebra$Math_Vector2$length(inputState.finalAim),
			_xarvh$elm_haifisch$Game$headingControlThreshold) < 0;
		var moveLength = _elm_community$linear_algebra$Math_Vector2$length(inputState.move);
		var move = (_elm_lang$core$Native_Utils.cmp(moveLength, 1) > 0) ? A2(_elm_community$linear_algebra$Math_Vector2$scale, 1 / moveLength, inputState.move) : inputState.move;
		var ignoreVelocityControl = _elm_lang$core$Native_Utils.cmp(moveLength, _xarvh$elm_haifisch$Game$velocityControlThreshold) < 0;
		var newPosition = function () {
			if (ignoreVelocityControl) {
				return ship.position;
			} else {
				var f = 0.85 + (0.15 * _elm_lang$core$Basics$cos(
					_xarvh$elm_haifisch$Common$vectorToAngle(move) - ship.heading));
				return A2(
					_xarvh$elm_haifisch$Common$clampToRadius,
					_xarvh$elm_haifisch$Game$worldRadius,
					A2(
						_elm_community$linear_algebra$Math_Vector2$add,
						A2(_elm_community$linear_algebra$Math_Vector2$scale, (f * _xarvh$elm_haifisch$Ship$speed) * dt, move),
						ship.position));
			}
		}();
		var targetHeading = ignoreHeadingControl ? (ignoreVelocityControl ? ship.heading : _xarvh$elm_haifisch$Common$vectorToAngle(move)) : _xarvh$elm_haifisch$Common$vectorToAngle(inputState.finalAim);
		var deltaHeading = _xarvh$elm_haifisch$Common$normalizeAngle(targetHeading - ship.heading);
		var clampedDeltaAngle = A3(_elm_lang$core$Basics$clamp, 0 - maxTurn, maxTurn, deltaHeading);
		var newHeading = _xarvh$elm_haifisch$Common$normalizeAngle(ship.heading + clampedDeltaAngle);
		return {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Native_Utils.update(
				ship,
				{position: newPosition, heading: newHeading}),
			_1: {ctor: '[]'}
		};
	});
var _xarvh$elm_haifisch$Game$shipTick = F3(
	function (inputStateByPlayerId, dt, ship) {
		var _p16 = A2(_elm_lang$core$Dict$get, ship.playerId, inputStateByPlayerId);
		if (_p16.ctor === 'Nothing') {
			return A2(_xarvh$elm_haifisch$Game$shipExplodeTick, dt, ship);
		} else {
			var _p18 = _p16._0;
			var _p17 = ship.status;
			switch (_p17.ctor) {
				case 'Spawning':
					return A2(
						_xarvh$elm_haifisch$Game_ops['|>>'],
						A3(_xarvh$elm_haifisch$Game$shipMovementControl, _p18, dt, ship),
						_xarvh$elm_haifisch$Game$shipSpawnTick(dt));
				case 'Active':
					return A2(
						_xarvh$elm_haifisch$Game_ops['|>>'],
						A3(_xarvh$elm_haifisch$Game$shipMovementControl, _p18, dt, ship),
						A2(_xarvh$elm_haifisch$Game$shipFireControl, _p18, dt));
				default:
					return A2(_xarvh$elm_haifisch$Game$shipExplodeTick, dt, ship);
			}
		}
	});
var _xarvh$elm_haifisch$Game$randomPosition = A3(
	_elm_lang$core$Random$map2,
	F2(
		function (r, a) {
			return A2(
				_elm_community$linear_algebra$Math_Vector2$vec2,
				r * _elm_lang$core$Basics$sin(a),
				r * _elm_lang$core$Basics$cos(a));
		}),
	A2(_elm_lang$core$Random$float, 0, _xarvh$elm_haifisch$Game$worldRadius),
	A2(
		_elm_lang$core$Random$float,
		0,
		_elm_lang$core$Basics$turns(1)));
var _xarvh$elm_haifisch$Game$randomShip = F2(
	function (playerId, colorKey) {
		return A3(
			_elm_lang$core$Random$map2,
			_xarvh$elm_haifisch$Game$makeShip(playerId),
			_xarvh$elm_haifisch$Game$randomPosition,
			_xarvh$elm_haifisch$Names$ship(colorKey));
	});
var _xarvh$elm_haifisch$Game$addShip = F2(
	function (player, model) {
		var _p19 = A2(
			_elm_lang$core$Random$step,
			A2(_xarvh$elm_haifisch$Game$randomShip, player.id, player.colorPattern.key),
			model.seed);
		var newShip = _p19._0;
		var newSeed = _p19._1;
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				shipsById: A3(_elm_lang$core$Dict$insert, player.id, newShip, model.shipsById),
				seed: newSeed
			});
	});
var _xarvh$elm_haifisch$Game$applyDelta = F2(
	function (effect, model) {
		var _p20 = effect;
		switch (_p20.ctor) {
			case 'AddProjectile':
				return _elm_lang$core$Native_Utils.update(
					model,
					{
						projectiles: {ctor: '::', _0: _p20._0, _1: model.projectiles}
					});
			case 'CreateShip':
				return A2(_xarvh$elm_haifisch$Game$addShip, _p20._0, model);
			case 'RemoveProjectile':
				return _elm_lang$core$Native_Utils.update(
					model,
					{
						projectiles: A2(_elm_community$list_extra$List_Extra$remove, _p20._0, model.projectiles)
					});
			case 'DamageShip':
				var _p21 = A2(_elm_lang$core$Dict$get, _p20._0, model.shipsById);
				if (_p21.ctor === 'Just') {
					return A2(
						_xarvh$elm_haifisch$Game$updateShip,
						_elm_lang$core$Native_Utils.update(
							_p21._0,
							{status: _xarvh$elm_haifisch$Common$Exploding}),
						model);
				} else {
					return model;
				}
			default:
				return _elm_lang$core$Native_Utils.update(
					model,
					{
						shipsById: A2(_elm_lang$core$Dict$remove, _p20._0, model.shipsById)
					});
		}
	});
var _xarvh$elm_haifisch$Game$projectileTick = F3(
	function (model, dt, oldProjectile) {
		var newPosition = A2(
			_elm_community$linear_algebra$Math_Vector2$add,
			oldProjectile.position,
			A2(
				_elm_community$linear_algebra$Math_Vector2$scale,
				_xarvh$elm_haifisch$Game$projectileSpeed * dt,
				_xarvh$elm_haifisch$Common$angleToVector(oldProjectile.heading)));
		var newProjectile = _elm_lang$core$Native_Utils.update(
			oldProjectile,
			{position: newPosition});
		var collisionWithShip = function (ship) {
			return A3(_xarvh$elm_haifisch$Collision$projectileVsShip, oldProjectile.position, newProjectile.position, ship);
		};
		var collideWithShip = F3(
			function (id, ship, deltas) {
				return ((!_elm_lang$core$Native_Utils.eq(ship.playerId, oldProjectile.playerId)) && (_elm_lang$core$Native_Utils.eq(ship.status, _xarvh$elm_haifisch$Common$Active) && collisionWithShip(ship))) ? A2(
					_elm_lang$core$Basics_ops['++'],
					{
						ctor: '::',
						_0: _xarvh$elm_haifisch$Common$D(
							_xarvh$elm_haifisch$Common$RemoveProjectile(newProjectile)),
						_1: {
							ctor: '::',
							_0: _xarvh$elm_haifisch$Common$D(
								_xarvh$elm_haifisch$Common$DamageShip(ship.playerId)),
							_1: {
								ctor: '::',
								_0: _xarvh$elm_haifisch$Common$E(
									_xarvh$elm_haifisch$Common$ShipExplodes(ship.playerId)),
								_1: {
									ctor: '::',
									_0: _xarvh$elm_haifisch$Common$E(
										A2(_xarvh$elm_haifisch$Common$ShipDamagesShip, oldProjectile.playerId, ship.playerId)),
									_1: {ctor: '[]'}
								}
							}
						}
					},
					deltas) : deltas;
			});
		var collisionEffets = A3(
			_elm_lang$core$Dict$foldl,
			collideWithShip,
			{ctor: '[]'},
			model.shipsById);
		var boundaryEffects = (_elm_lang$core$Native_Utils.cmp(
			_elm_community$linear_algebra$Math_Vector2$length(newPosition),
			_xarvh$elm_haifisch$Game$worldRadius) > 0) ? {
			ctor: '::',
			_0: _xarvh$elm_haifisch$Common$D(
				_xarvh$elm_haifisch$Common$RemoveProjectile(newProjectile)),
			_1: {ctor: '[]'}
		} : {ctor: '[]'};
		return {
			ctor: '_Tuple2',
			_0: newProjectile,
			_1: A2(_elm_lang$core$Basics_ops['++'], collisionEffets, boundaryEffects)
		};
	});
var _xarvh$elm_haifisch$Game$tick = F3(
	function (inputStateByPlayerId, dt, oldModel) {
		var tickedPlanets = A2(
			_elm_lang$core$List$map,
			_xarvh$elm_haifisch$Game$planetTick(dt),
			oldModel.planets);
		var _p22 = A2(
			_xarvh$elm_haifisch$Game$outcomesOverList,
			A2(_xarvh$elm_haifisch$Game$playerTick, oldModel, inputStateByPlayerId),
			oldModel.players);
		var tickedPlayers = _p22._0;
		var playersOutcomes = _p22._1;
		var _p23 = A2(
			_xarvh$elm_haifisch$Game$outcomesOverList,
			A2(_xarvh$elm_haifisch$Game$projectileTick, oldModel, dt),
			oldModel.projectiles);
		var tickedProjectiles = _p23._0;
		var projectileOutcomes = _p23._1;
		var _p24 = A2(
			_xarvh$elm_haifisch$Game$outcomesOverDict,
			A2(_xarvh$elm_haifisch$Game$shipTick, inputStateByPlayerId, dt),
			oldModel.shipsById);
		var tickedShipsById = _p24._0;
		var shipOutcomes = _p24._1;
		var tickedModel = _elm_lang$core$Native_Utils.update(
			oldModel,
			{shipsById: tickedShipsById, projectiles: tickedProjectiles, planets: tickedPlanets, players: tickedPlayers});
		var _p25 = _xarvh$elm_haifisch$Game$splitOutcomes(
			A2(
				_elm_lang$core$Basics_ops['++'],
				shipOutcomes,
				A2(_elm_lang$core$Basics_ops['++'], projectileOutcomes, playersOutcomes)));
		var deltas = _p25._0;
		var events = _p25._1;
		var newModel = A3(_elm_lang$core$List$foldl, _xarvh$elm_haifisch$Game$applyDelta, tickedModel, deltas);
		return {ctor: '_Tuple2', _0: newModel, _1: events};
	});
var _xarvh$elm_haifisch$Game$Model = F6(
	function (a, b, c, d, e, f) {
		return {planets: a, players: b, projectiles: c, seed: d, shipsById: e, shuffledColorPatterns: f};
	});

var _xarvh$elm_haifisch$Bots$wrapModel = F2(
	function (model, _p0) {
		var _p1 = _p0;
		return {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Native_Utils.update(
				model,
				{state: _p1._0}),
			_1: _p1._1
		};
	});
var _xarvh$elm_haifisch$Bots$addBot = F2(
	function (state, bots) {
		var id = A2(
			F2(
				function (x, y) {
					return x + y;
				}),
			1,
			A2(
				_elm_lang$core$Maybe$withDefault,
				0,
				_elm_lang$core$List$maximum(
					A2(
						_elm_lang$core$List$map,
						function (_) {
							return _.id;
						},
						bots))));
		var bot = {id: id, state: state};
		return {ctor: '::', _0: bot, _1: bots};
	});
var _xarvh$elm_haifisch$Bots$StaticBot = {};
var _xarvh$elm_haifisch$Bots$FullRandomBot = function (a) {
	return {seed: a};
};
var _xarvh$elm_haifisch$Bots$Model = F2(
	function (a, b) {
		return {id: a, state: b};
	});
var _xarvh$elm_haifisch$Bots$StateStaticBot = function (a) {
	return {ctor: 'StateStaticBot', _0: a};
};
var _xarvh$elm_haifisch$Bots$staticBot = function (game) {
	return _xarvh$elm_haifisch$Bots$StateStaticBot(
		{});
};
var _xarvh$elm_haifisch$Bots$staticBotThink = F2(
	function (game, model) {
		return {
			ctor: '_Tuple2',
			_0: _xarvh$elm_haifisch$Bots$StateStaticBot(model),
			_1: {
				finalAim: A2(_elm_community$linear_algebra$Math_Vector2$vec2, 0, 0),
				fire: true,
				move: A2(_elm_community$linear_algebra$Math_Vector2$vec2, 0, 0)
			}
		};
	});
var _xarvh$elm_haifisch$Bots$StateFullRandomBot = function (a) {
	return {ctor: 'StateFullRandomBot', _0: a};
};
var _xarvh$elm_haifisch$Bots$fullRandomBot = function (game) {
	return _xarvh$elm_haifisch$Bots$StateFullRandomBot(
		{seed: game.seed});
};
var _xarvh$elm_haifisch$Bots$fullRandomBotThink = F2(
	function (game, model) {
		var vec2Generator = A3(
			_elm_lang$core$Random$map2,
			_elm_community$linear_algebra$Math_Vector2$vec2,
			A2(_elm_lang$core$Random$float, -1, 1),
			A2(_elm_lang$core$Random$float, -1, 1));
		var inputStateGenerator = A4(_elm_lang$core$Random$map3, _xarvh$elm_haifisch$Common$InputState, vec2Generator, _elm_lang$core$Random$bool, vec2Generator);
		var _p2 = A2(_elm_lang$core$Random$step, inputStateGenerator, model.seed);
		var inputState = _p2._0;
		var seed = _p2._1;
		return {
			ctor: '_Tuple2',
			_0: _xarvh$elm_haifisch$Bots$StateFullRandomBot(
				{seed: seed}),
			_1: inputState
		};
	});
var _xarvh$elm_haifisch$Bots$think = F2(
	function (game, model) {
		return A2(
			_xarvh$elm_haifisch$Bots$wrapModel,
			model,
			function () {
				var _p3 = model.state;
				if (_p3.ctor === 'StateStaticBot') {
					return A2(_xarvh$elm_haifisch$Bots$staticBotThink, game, _p3._0);
				} else {
					return A2(_xarvh$elm_haifisch$Bots$fullRandomBotThink, game, _p3._0);
				}
			}());
	});

var _xarvh$elm_haifisch$GamepadPort$gamepad = _elm_lang$core$Native_Platform.incomingPort(
	'gamepad',
	A2(
		_elm_lang$core$Json_Decode$andThen,
		function (x0) {
			return A2(
				_elm_lang$core$Json_Decode$andThen,
				function (x1) {
					return _elm_lang$core$Json_Decode$succeed(
						{ctor: '_Tuple2', _0: x0, _1: x1});
				},
				A2(
					_elm_lang$core$Json_Decode$index,
					1,
					_elm_lang$core$Json_Decode$list(
						_elm_lang$core$Json_Decode$oneOf(
							{
								ctor: '::',
								_0: _elm_lang$core$Json_Decode$null(_elm_lang$core$Maybe$Nothing),
								_1: {
									ctor: '::',
									_0: A2(
										_elm_lang$core$Json_Decode$map,
										_elm_lang$core$Maybe$Just,
										A2(
											_elm_lang$core$Json_Decode$andThen,
											function (axes) {
												return A2(
													_elm_lang$core$Json_Decode$andThen,
													function (buttons) {
														return A2(
															_elm_lang$core$Json_Decode$andThen,
															function (connected) {
																return A2(
																	_elm_lang$core$Json_Decode$andThen,
																	function (id) {
																		return A2(
																			_elm_lang$core$Json_Decode$andThen,
																			function (index) {
																				return A2(
																					_elm_lang$core$Json_Decode$andThen,
																					function (mapping) {
																						return A2(
																							_elm_lang$core$Json_Decode$andThen,
																							function (timestamp) {
																								return _elm_lang$core$Json_Decode$succeed(
																									{axes: axes, buttons: buttons, connected: connected, id: id, index: index, mapping: mapping, timestamp: timestamp});
																							},
																							A2(_elm_lang$core$Json_Decode$field, 'timestamp', _elm_lang$core$Json_Decode$float));
																					},
																					A2(_elm_lang$core$Json_Decode$field, 'mapping', _elm_lang$core$Json_Decode$string));
																			},
																			A2(_elm_lang$core$Json_Decode$field, 'index', _elm_lang$core$Json_Decode$int));
																	},
																	A2(_elm_lang$core$Json_Decode$field, 'id', _elm_lang$core$Json_Decode$string));
															},
															A2(_elm_lang$core$Json_Decode$field, 'connected', _elm_lang$core$Json_Decode$bool));
													},
													A2(
														_elm_lang$core$Json_Decode$field,
														'buttons',
														_elm_lang$core$Json_Decode$array(
															A2(
																_elm_lang$core$Json_Decode$andThen,
																function (x0) {
																	return A2(
																		_elm_lang$core$Json_Decode$andThen,
																		function (x1) {
																			return _elm_lang$core$Json_Decode$succeed(
																				{ctor: '_Tuple2', _0: x0, _1: x1});
																		},
																		A2(_elm_lang$core$Json_Decode$index, 1, _elm_lang$core$Json_Decode$float));
																},
																A2(_elm_lang$core$Json_Decode$index, 0, _elm_lang$core$Json_Decode$bool)))));
											},
											A2(
												_elm_lang$core$Json_Decode$field,
												'axes',
												_elm_lang$core$Json_Decode$array(_elm_lang$core$Json_Decode$float)))),
									_1: {ctor: '[]'}
								}
							}))));
		},
		A2(_elm_lang$core$Json_Decode$index, 0, _elm_lang$core$Json_Decode$float)));

var _xarvh$elm_haifisch$MousePort$mouseButton = _elm_lang$core$Native_Platform.incomingPort(
	'mouseButton',
	A2(
		_elm_lang$core$Json_Decode$andThen,
		function (x0) {
			return A2(
				_elm_lang$core$Json_Decode$andThen,
				function (x1) {
					return _elm_lang$core$Json_Decode$succeed(
						{ctor: '_Tuple2', _0: x0, _1: x1});
				},
				A2(_elm_lang$core$Json_Decode$index, 1, _elm_lang$core$Json_Decode$bool));
		},
		A2(_elm_lang$core$Json_Decode$index, 0, _elm_lang$core$Json_Decode$int)));
var _xarvh$elm_haifisch$MousePort$button = _xarvh$elm_haifisch$MousePort$mouseButton;

var _xarvh$elm_haifisch$Input$update = F2(
	function (msg, model) {
		var _p0 = msg;
		switch (_p0.ctor) {
			case 'OnKeyboardMsg':
				return _elm_lang$core$Native_Utils.update(
					model,
					{
						pressedKeys: A2(_ohanhi$keyboard_extra$Keyboard_Extra$update, _p0._0, model.pressedKeys)
					});
			case 'OnMouseButton':
				var _p2 = _p0._0._1;
				var _p1 = _p0._0._0;
				switch (_p1) {
					case 0:
						return _elm_lang$core$Native_Utils.update(
							model,
							{mouseButtonLeft: _p2});
					case 1:
						return _elm_lang$core$Native_Utils.update(
							model,
							{mouseButtonMiddle: _p2});
					default:
						return _elm_lang$core$Native_Utils.update(
							model,
							{mouseButtonRight: _p2});
				}
			default:
				return _elm_lang$core$Native_Utils.update(
					model,
					{mousePosition: _p0._0});
		}
	});
var _xarvh$elm_haifisch$Input$init = {
	mousePosition: A2(_elm_lang$mouse$Mouse$Position, 0, 0),
	mouseButtonLeft: false,
	mouseButtonMiddle: false,
	mouseButtonRight: false,
	pressedKeys: {ctor: '[]'}
};
var _xarvh$elm_haifisch$Input$v0 = A2(_elm_community$linear_algebra$Math_Vector2$vec2, 0, 0);
var _xarvh$elm_haifisch$Input$RawInputState = F3(
	function (a, b, c) {
		return {finalAim: a, fire: b, move: c};
	});
var _xarvh$elm_haifisch$Input$Model = F5(
	function (a, b, c, d, e) {
		return {mousePosition: a, mouseButtonLeft: b, mouseButtonMiddle: c, mouseButtonRight: d, pressedKeys: e};
	});
var _xarvh$elm_haifisch$Input$SourceGamepad = function (a) {
	return {ctor: 'SourceGamepad', _0: a};
};
var _xarvh$elm_haifisch$Input$SourceKeyboardAndMouse = {ctor: 'SourceKeyboardAndMouse'};
var _xarvh$elm_haifisch$Input$OnePlayerUsesKeyboardAndMouse = {ctor: 'OnePlayerUsesKeyboardAndMouse'};
var _xarvh$elm_haifisch$Input$AllPlayersUseGamepads = {ctor: 'AllPlayersUseGamepads'};
var _xarvh$elm_haifisch$Input$ScreenPosition = function (a) {
	return {ctor: 'ScreenPosition', _0: a};
};
var _xarvh$elm_haifisch$Input$keyboardAndMouseToInputState = function (model) {
	var _p3 = _ohanhi$keyboard_extra$Keyboard_Extra$wasd(model.pressedKeys);
	var x = _p3.x;
	var y = _p3.y;
	var move = A2(
		_elm_community$linear_algebra$Math_Vector2$vec2,
		_elm_lang$core$Basics$toFloat(x),
		_elm_lang$core$Basics$toFloat(y));
	var pixelsForAFullTurn = 2000;
	var turningRatio = _elm_lang$core$Basics$turns(1) / pixelsForAFullTurn;
	return {
		finalAim: _xarvh$elm_haifisch$Input$ScreenPosition(model.mousePosition),
		fire: model.mouseButtonLeft,
		move: move
	};
};
var _xarvh$elm_haifisch$Input$Direction = function (a) {
	return {ctor: 'Direction', _0: a};
};
var _xarvh$elm_haifisch$Input$gamepadToInputState = function (gamepad) {
	var move = A2(
		_elm_community$linear_algebra$Math_Vector2$vec2,
		_xarvh$elm_gamepad$Gamepad$leftX(gamepad),
		_xarvh$elm_gamepad$Gamepad$leftY(gamepad));
	var fire = _xarvh$elm_gamepad$Gamepad$aIsPressed(gamepad);
	var aimDirection = A2(
		_elm_community$linear_algebra$Math_Vector2$vec2,
		_xarvh$elm_gamepad$Gamepad$rightX(gamepad),
		_xarvh$elm_gamepad$Gamepad$rightY(gamepad));
	return {
		finalAim: _xarvh$elm_haifisch$Input$Direction(aimDirection),
		fire: fire,
		move: move
	};
};
var _xarvh$elm_haifisch$Input$sourcesAndStates = F3(
	function (maybeConfig, gamepads, model) {
		var gamepadToTuple = function (gamepad) {
			return {
				ctor: '_Tuple2',
				_0: _xarvh$elm_haifisch$Input$SourceGamepad(
					_xarvh$elm_gamepad$Gamepad$getIndex(gamepad)),
				_1: _xarvh$elm_haifisch$Input$gamepadToInputState(gamepad)
			};
		};
		var gamepadInputs = A2(_elm_lang$core$List$map, gamepadToTuple, gamepads);
		var config = function () {
			var _p4 = maybeConfig;
			if (_p4.ctor === 'Just') {
				return _p4._0;
			} else {
				return (_elm_lang$core$Native_Utils.cmp(
					_elm_lang$core$List$length(gamepads),
					0) > 0) ? _xarvh$elm_haifisch$Input$AllPlayersUseGamepads : _xarvh$elm_haifisch$Input$OnePlayerUsesKeyboardAndMouse;
			}
		}();
		var keyboardInputs = function () {
			var _p5 = config;
			if (_p5.ctor === 'AllPlayersUseGamepads') {
				return {ctor: '[]'};
			} else {
				return {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: _xarvh$elm_haifisch$Input$SourceKeyboardAndMouse,
						_1: _xarvh$elm_haifisch$Input$keyboardAndMouseToInputState(model)
					},
					_1: {ctor: '[]'}
				};
			}
		}();
		return A2(_elm_lang$core$Basics_ops['++'], keyboardInputs, gamepadInputs);
	});
var _xarvh$elm_haifisch$Input$OnMouseMove = function (a) {
	return {ctor: 'OnMouseMove', _0: a};
};
var _xarvh$elm_haifisch$Input$OnMouseButton = function (a) {
	return {ctor: 'OnMouseButton', _0: a};
};
var _xarvh$elm_haifisch$Input$OnKeyboardMsg = function (a) {
	return {ctor: 'OnKeyboardMsg', _0: a};
};
var _xarvh$elm_haifisch$Input$subscriptions = function (model) {
	return _elm_lang$core$Platform_Sub$batch(
		{
			ctor: '::',
			_0: A2(_elm_lang$core$Platform_Sub$map, _xarvh$elm_haifisch$Input$OnKeyboardMsg, _ohanhi$keyboard_extra$Keyboard_Extra$subscriptions),
			_1: {
				ctor: '::',
				_0: _elm_lang$mouse$Mouse$moves(_xarvh$elm_haifisch$Input$OnMouseMove),
				_1: {
					ctor: '::',
					_0: _xarvh$elm_haifisch$MousePort$button(_xarvh$elm_haifisch$Input$OnMouseButton),
					_1: {ctor: '[]'}
				}
			}
		});
};

var _xarvh$elm_haifisch$LocalStoragePort$localStorageSet = _elm_lang$core$Native_Platform.outgoingPort(
	'localStorageSet',
	function (v) {
		return {key: v.key, value: v.value};
	});
var _xarvh$elm_haifisch$LocalStoragePort$set = F2(
	function (key, value) {
		return _xarvh$elm_haifisch$LocalStoragePort$localStorageSet(
			{key: key, value: value});
	});

var _xarvh$elm_haifisch$SoundPort$playSound = _elm_lang$core$Native_Platform.outgoingPort(
	'playSound',
	function (v) {
		return v;
	});

var _xarvh$elm_haifisch$View$viewbox = function (worldSize) {
	var _p0 = _elm_community$linear_algebra$Math_Vector2$toTuple(worldSize);
	var w = _p0._0;
	var h = _p0._1;
	return A2(
		_elm_lang$core$String$join,
		' ',
		A2(
			_elm_lang$core$List$map,
			_elm_lang$core$Basics$toString,
			{
				ctor: '::',
				_0: (0 - w) / 2,
				_1: {
					ctor: '::',
					_0: (0 - h) / 2,
					_1: {
						ctor: '::',
						_0: w,
						_1: {
							ctor: '::',
							_0: h,
							_1: {ctor: '[]'}
						}
					}
				}
			}));
};
var _xarvh$elm_haifisch$View$splash = F2(
	function (title, content) {
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('splash-container full-window'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$div,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('splash'),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$h2,
							{ctor: '[]'},
							{
								ctor: '::',
								_0: _elm_lang$html$Html$text(title),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$div,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('splash-content'),
									_1: {ctor: '[]'}
								},
								content),
							_1: {ctor: '[]'}
						}
					}),
				_1: {ctor: '[]'}
			});
	});
var _xarvh$elm_haifisch$View$worldRadius = _xarvh$elm_haifisch$Game$worldRadius;
var _xarvh$elm_haifisch$View$projectileRadius = 1.0e-2 * _xarvh$elm_haifisch$View$worldRadius;
var _xarvh$elm_haifisch$View$shipStrokWidth = 5.0e-3 * _xarvh$elm_haifisch$View$worldRadius;
var _xarvh$elm_haifisch$View$shipMesh = F3(
	function (opacity, colorPattern, ship) {
		var vertices = A2(
			_elm_lang$core$List$map,
			_xarvh$elm_haifisch$Common$vectorToString,
			A2(_xarvh$elm_haifisch$Ship$transform, ship, _xarvh$elm_haifisch$Ship$mesh));
		var path = A2(
			_elm_lang$core$Basics_ops['++'],
			'M ',
			A2(
				_elm_lang$core$Basics_ops['++'],
				A2(_elm_lang$core$String$join, ' L ', vertices),
				' Z'));
		return A2(
			_elm_lang$svg$Svg$path,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$d(path),
				_1: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$style(
						A2(
							_elm_lang$core$String$join,
							'; ',
							{
								ctor: '::',
								_0: A2(_elm_lang$core$Basics_ops['++'], 'fill: ', colorPattern.dark),
								_1: {
									ctor: '::',
									_0: A2(_elm_lang$core$Basics_ops['++'], 'stroke: ', colorPattern.bright),
									_1: {
										ctor: '::',
										_0: A2(
											_elm_lang$core$Basics_ops['++'],
											'stroke-width: ',
											_elm_lang$core$Basics$toString(_xarvh$elm_haifisch$View$shipStrokWidth)),
										_1: {
											ctor: '::',
											_0: A2(
												_elm_lang$core$Basics_ops['++'],
												'opacity: ',
												_elm_lang$core$Basics$toString(opacity)),
											_1: {ctor: '[]'}
										}
									}
								}
							})),
					_1: {ctor: '[]'}
				}
			},
			{ctor: '[]'});
	});
var _xarvh$elm_haifisch$View$star = A2(
	_elm_lang$svg$Svg$circle,
	{
		ctor: '::',
		_0: _elm_lang$svg$Svg_Attributes$cx('0'),
		_1: {
			ctor: '::',
			_0: _elm_lang$svg$Svg_Attributes$cy('0'),
			_1: {
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$r(
					_elm_lang$core$Basics$toString(2.0e-2 * _xarvh$elm_haifisch$View$worldRadius)),
				_1: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$fill('#eee'),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$stroke('#aaa'),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$strokeWidth(
								_elm_lang$core$Basics$toString(2.0e-3 * _xarvh$elm_haifisch$View$worldRadius)),
							_1: {ctor: '[]'}
						}
					}
				}
			}
		}
	},
	{ctor: '[]'});
var _xarvh$elm_haifisch$View$satellite = function (s) {
	return A2(
		_elm_lang$svg$Svg$g,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$svg$Svg$circle,
				{
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$cx('0'),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$cy('0'),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$r(
								_elm_lang$core$Basics$toString(s.orbitRadius)),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$fill('none'),
								_1: {
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$stroke('#ddd'),
									_1: {
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$strokeWidth(
											_elm_lang$core$Basics$toString(2.0e-3 * _xarvh$elm_haifisch$View$worldRadius)),
										_1: {ctor: '[]'}
									}
								}
							}
						}
					}
				},
				{ctor: '[]'}),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$svg$Svg$circle,
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$cx(
							_elm_lang$core$Basics$toString(s.orbitRadius)),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$cy('0'),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$r(
									_elm_lang$core$Basics$toString(3.0e-3 * _xarvh$elm_haifisch$View$worldRadius)),
								_1: {
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$fill('#777'),
									_1: {
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$stroke('#ddd'),
										_1: {
											ctor: '::',
											_0: _elm_lang$svg$Svg_Attributes$strokeWidth(
												_elm_lang$core$Basics$toString(2.0e-3 * _xarvh$elm_haifisch$View$worldRadius)),
											_1: {
												ctor: '::',
												_0: _elm_lang$svg$Svg_Attributes$transform(
													A2(
														_elm_lang$core$Basics_ops['++'],
														'rotate(',
														A2(
															_elm_lang$core$Basics_ops['++'],
															_elm_lang$core$Basics$toString(
																s.angle / _elm_lang$core$Basics$degrees(1)),
															')'))),
												_1: {ctor: '[]'}
											}
										}
									}
								}
							}
						}
					},
					{ctor: '[]'}),
				_1: {ctor: '[]'}
			}
		});
};
var _xarvh$elm_haifisch$View$planet = function (p) {
	var satellites = A2(_elm_lang$core$List$map, _xarvh$elm_haifisch$View$satellite, p.satellites);
	var planet = A2(
		_elm_lang$svg$Svg$circle,
		{
			ctor: '::',
			_0: _elm_lang$svg$Svg_Attributes$cx('0'),
			_1: {
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$cy('0'),
				_1: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$r(
						_elm_lang$core$Basics$toString(p.surfaceRadius)),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$fill('#777'),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$stroke('#ddd'),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$strokeWidth(
									_elm_lang$core$Basics$toString(2.0e-3 * _xarvh$elm_haifisch$View$worldRadius)),
								_1: {ctor: '[]'}
							}
						}
					}
				}
			}
		},
		{ctor: '[]'});
	var orbit = A2(
		_elm_lang$svg$Svg$circle,
		{
			ctor: '::',
			_0: _elm_lang$svg$Svg_Attributes$cx('0'),
			_1: {
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$cy('0'),
				_1: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$r(
						_elm_lang$core$Basics$toString(p.orbitRadius)),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$fill('none'),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$stroke('#ddd'),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$strokeWidth(
									_elm_lang$core$Basics$toString(2.0e-3 * _xarvh$elm_haifisch$View$worldRadius)),
								_1: {ctor: '[]'}
							}
						}
					}
				}
			}
		},
		{ctor: '[]'});
	var y = (p.orbitRadius * -1) * _elm_lang$core$Basics$sin(p.angle);
	var x = p.orbitRadius * _elm_lang$core$Basics$cos(p.angle);
	var transform = A2(
		_elm_lang$core$Basics_ops['++'],
		'translate(',
		A2(
			_elm_lang$core$Basics_ops['++'],
			_elm_lang$core$Basics$toString(x),
			A2(
				_elm_lang$core$Basics_ops['++'],
				', ',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(y),
					')'))));
	return A2(
		_elm_lang$svg$Svg$g,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: orbit,
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$svg$Svg$g,
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$transform(transform),
						_1: {ctor: '[]'}
					},
					{ctor: '::', _0: planet, _1: satellites}),
				_1: {ctor: '[]'}
			}
		});
};
var _xarvh$elm_haifisch$View$outerWellMarker = A2(
	_elm_lang$svg$Svg$circle,
	{
		ctor: '::',
		_0: _elm_lang$svg$Svg_Attributes$cx('0'),
		_1: {
			ctor: '::',
			_0: _elm_lang$svg$Svg_Attributes$cy('0'),
			_1: {
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$r(
					_elm_lang$core$Basics$toString(_xarvh$elm_haifisch$View$worldRadius)),
				_1: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$fill('none'),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$stroke('#aaa'),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$strokeWidth(
								_elm_lang$core$Basics$toString(6.0e-3 * _xarvh$elm_haifisch$View$worldRadius)),
							_1: {ctor: '[]'}
						}
					}
				}
			}
		}
	},
	{ctor: '[]'});
var _xarvh$elm_haifisch$View$ship = F2(
	function (playersById, ship) {
		var colorPattern = A2(
			_elm_lang$core$Maybe$withDefault,
			_xarvh$elm_haifisch$ColorPattern$neutral,
			A2(
				_elm_lang$core$Maybe$map,
				function (_) {
					return _.colorPattern;
				},
				A2(_elm_lang$core$Dict$get, ship.playerId, playersById)));
		var _p1 = ship.status;
		switch (_p1.ctor) {
			case 'Active':
				return A3(_xarvh$elm_haifisch$View$shipMesh, 1.0, colorPattern, ship);
			case 'Spawning':
				var blinkPeriod = 0.25 * _elm_lang$core$Time$second;
				var phase = ship.respawnTime / blinkPeriod;
				var normalPhase = phase - _elm_lang$core$Basics$toFloat(
					_elm_lang$core$Basics$floor(phase));
				var angularPhase = normalPhase * _elm_lang$core$Basics$turns(1);
				var opacity = (1 + _elm_lang$core$Basics$sin(angularPhase)) / 2;
				return A3(_xarvh$elm_haifisch$View$shipMesh, opacity, colorPattern, ship);
			default:
				var r = 0.1 * _xarvh$elm_haifisch$View$worldRadius;
				var n = 5;
				var t = ship.explodeTime / _xarvh$elm_haifisch$Ship$explosionDuration;
				var particleByIndex = function (index) {
					var a = _elm_lang$core$Basics$turns(index / n);
					var x = (t * r) * _elm_lang$core$Basics$cos(a);
					var y = (t * r) * _elm_lang$core$Basics$sin(a);
					return A2(
						_elm_lang$svg$Svg$circle,
						{
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$cx(
								_elm_lang$core$Basics$toString(x)),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$cy(
									_elm_lang$core$Basics$toString(y)),
								_1: {
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$r(
										_elm_lang$core$Basics$toString((((t * 0.9) + 0.1) * 0.2) * _xarvh$elm_haifisch$View$worldRadius)),
									_1: {
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$opacity(
											_elm_lang$core$Basics$toString((1 - t) / 3)),
										_1: {
											ctor: '::',
											_0: _elm_lang$svg$Svg_Attributes$fill(colorPattern.dark),
											_1: {
												ctor: '::',
												_0: _elm_lang$svg$Svg_Attributes$stroke(colorPattern.bright),
												_1: {
													ctor: '::',
													_0: _elm_lang$svg$Svg_Attributes$strokeWidth(
														_elm_lang$core$Basics$toString(_xarvh$elm_haifisch$View$shipStrokWidth * 2)),
													_1: {ctor: '[]'}
												}
											}
										}
									}
								}
							}
						},
						{ctor: '[]'});
				};
				return A2(
					_elm_lang$svg$Svg$g,
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$transform(
							A2(
								_elm_lang$core$Basics_ops['++'],
								'translate(',
								A2(
									_elm_lang$core$Basics_ops['++'],
									_xarvh$elm_haifisch$Common$vectorToString(ship.position),
									')'))),
						_1: {ctor: '[]'}
					},
					A2(
						_elm_lang$core$List$map,
						particleByIndex,
						A2(
							_elm_lang$core$List$map,
							_elm_lang$core$Basics$toFloat,
							A2(_elm_lang$core$List$range, 0, n - 1))));
		}
	});
var _xarvh$elm_haifisch$View$projectile = F2(
	function (playersById, projectile) {
		var _p2 = _elm_community$linear_algebra$Math_Vector2$toTuple(projectile.position);
		var x = _p2._0;
		var y = _p2._1;
		var size = 1.0e-2 * _xarvh$elm_haifisch$View$worldRadius;
		var colorPattern = A2(
			_elm_lang$core$Maybe$withDefault,
			_xarvh$elm_haifisch$ColorPattern$neutral,
			A2(
				_elm_lang$core$Maybe$map,
				function (_) {
					return _.colorPattern;
				},
				A2(_elm_lang$core$Dict$get, projectile.playerId, playersById)));
		return A2(
			_elm_lang$svg$Svg$g,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$svg$Svg$circle,
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$cx(
							_elm_lang$core$Basics$toString(x)),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$cy(
								_elm_lang$core$Basics$toString(y)),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$r(
									_elm_lang$core$Basics$toString(1.0e-2 * _xarvh$elm_haifisch$View$worldRadius)),
								_1: {
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$fill(colorPattern.bright),
									_1: {
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$stroke(colorPattern.dark),
										_1: {
											ctor: '::',
											_0: _elm_lang$svg$Svg_Attributes$strokeWidth(
												_elm_lang$core$Basics$toString(1.0e-3 * _xarvh$elm_haifisch$View$worldRadius)),
											_1: {ctor: '[]'}
										}
									}
								}
							}
						}
					},
					{ctor: '[]'}),
				_1: {ctor: '[]'}
			});
	});
var _xarvh$elm_haifisch$View$game = F2(
	function (worldSize, model) {
		var playerIds = A2(
			_elm_lang$core$List$map,
			function (_) {
				return _.id;
			},
			model.players);
		var playersById = _elm_lang$core$Dict$fromList(
			A3(
				_elm_lang$core$List$map2,
				F2(
					function (v0, v1) {
						return {ctor: '_Tuple2', _0: v0, _1: v1};
					}),
				playerIds,
				model.players));
		var entities = {
			ctor: '::',
			_0: {
				ctor: '::',
				_0: _xarvh$elm_haifisch$View$star,
				_1: {
					ctor: '::',
					_0: _xarvh$elm_haifisch$View$outerWellMarker,
					_1: {ctor: '[]'}
				}
			},
			_1: {
				ctor: '::',
				_0: A2(_elm_lang$core$List$map, _xarvh$elm_haifisch$View$planet, model.planets),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$core$List$map,
						_xarvh$elm_haifisch$View$ship(playersById),
						_elm_lang$core$Dict$values(model.shipsById)),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$core$List$map,
							_xarvh$elm_haifisch$View$projectile(playersById),
							model.projectiles),
						_1: {ctor: '[]'}
					}
				}
			}
		};
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('star-system-container full-window'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$svg$Svg$svg,
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$viewBox(
							_xarvh$elm_haifisch$View$viewbox(worldSize)),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: A2(
							_elm_lang$svg$Svg$g,
							{
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$transform('scale(1, -1)'),
								_1: {ctor: '[]'}
							},
							_elm_lang$core$List$concat(entities)),
						_1: {ctor: '[]'}
					}),
				_1: {ctor: '[]'}
			});
	});

var _xarvh$elm_haifisch$View_Background$view = function () {
	var _p0 = {ctor: '_Tuple3', _0: '#ffffff', _1: '#f7f7f7', _2: '4px'};
	var fill = _p0._0;
	var stroke = _p0._1;
	var strokeWidth = _p0._2;
	return A2(
		_elm_lang$svg$Svg$svg,
		{
			ctor: '::',
			_0: _elm_lang$svg$Svg_Attributes$class('full-window'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$svg$Svg$defs,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: A2(
						_elm_lang$svg$Svg$pattern,
						{
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$id('hex-background'),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$width('56px'),
								_1: {
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$height('100px'),
									_1: {
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$patternUnits('userSpaceOnUse'),
										_1: {
											ctor: '::',
											_0: _elm_lang$svg$Svg_Attributes$patternTransform('scale(0.5)'),
											_1: {ctor: '[]'}
										}
									}
								}
							}
						},
						{
							ctor: '::',
							_0: A2(
								_elm_lang$svg$Svg$rect,
								{
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$width('100%'),
									_1: {
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$height('100%'),
										_1: {
											ctor: '::',
											_0: _elm_lang$svg$Svg_Attributes$fill(fill),
											_1: {ctor: '[]'}
										}
									}
								},
								{ctor: '[]'}),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_lang$svg$Svg$path,
									{
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$d('M28 66L0 50L0 16L28 0L56 16L56 50L28 66L28 100'),
										_1: {
											ctor: '::',
											_0: _elm_lang$svg$Svg_Attributes$fill(fill),
											_1: {
												ctor: '::',
												_0: _elm_lang$svg$Svg_Attributes$stroke(stroke),
												_1: {
													ctor: '::',
													_0: _elm_lang$svg$Svg_Attributes$strokeWidth(strokeWidth),
													_1: {ctor: '[]'}
												}
											}
										}
									},
									{ctor: '[]'}),
								_1: {ctor: '[]'}
							}
						}),
					_1: {ctor: '[]'}
				}),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$svg$Svg$rect,
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$fill('url(#hex-background)'),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$width('100%'),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$height('100%'),
								_1: {ctor: '[]'}
							}
						}
					},
					{ctor: '[]'}),
				_1: {ctor: '[]'}
			}
		});
}();

var _xarvh$elm_haifisch$View_Scoreboard$shipName = F2(
	function (player, shipsById) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			'',
			A2(
				_elm_lang$core$Maybe$map,
				function (_p0) {
					return function (_) {
						return _.name;
					}(
						_elm_lang$core$Tuple$second(_p0));
				},
				A2(
					_elm_community$dict_extra$Dict_Extra$find,
					F2(
						function (shipId, ship) {
							return _elm_lang$core$Native_Utils.eq(ship.playerId, player.id);
						}),
					shipsById)));
	});
var _xarvh$elm_haifisch$View_Scoreboard$textColor = function (color) {
	return _elm_lang$html$Html_Attributes$style(
		{
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: 'color', _1: color},
			_1: {ctor: '[]'}
		});
};
var _xarvh$elm_haifisch$View_Scoreboard$score = F2(
	function (shipsById, player) {
		return A2(
			_elm_lang$html$Html$li,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$p,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('score'),
						_1: {
							ctor: '::',
							_0: _xarvh$elm_haifisch$View_Scoreboard$textColor(player.colorPattern.bright),
							_1: {ctor: '[]'}
						}
					},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text(
							_elm_lang$core$Basics$toString(player.score)),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$p,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('name'),
							_1: {
								ctor: '::',
								_0: _xarvh$elm_haifisch$View_Scoreboard$textColor(player.colorPattern.bright),
								_1: {ctor: '[]'}
							}
						},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text(
								A2(_xarvh$elm_haifisch$View_Scoreboard$shipName, player, shipsById)),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			});
	});
var _xarvh$elm_haifisch$View_Scoreboard$scoreboard = F2(
	function (players, shipsById) {
		var visiblePlayers = A2(
			_elm_lang$core$List$sortBy,
			function (_) {
				return _.id;
			},
			A2(
				_elm_lang$core$List$filter,
				function (_) {
					return _.isConnected;
				},
				players));
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('scoreboard-container'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$ul,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('scoreboard'),
						_1: {ctor: '[]'}
					},
					A2(
						_elm_lang$core$List$map,
						_xarvh$elm_haifisch$View_Scoreboard$score(shipsById),
						visiblePlayers)),
				_1: {ctor: '[]'}
			});
	});

var _xarvh$elm_haifisch$App$view = function (model) {
	return A2(
		_elm_lang$html$Html$div,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _xarvh$elm_haifisch$View_Background$view,
			_1: {
				ctor: '::',
				_0: A2(_xarvh$elm_haifisch$View$game, model.windowSizeInGameCoordinates, model.game),
				_1: {
					ctor: '::',
					_0: A2(_xarvh$elm_haifisch$View_Scoreboard$scoreboard, model.game.players, model.game.shipsById),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _xarvh$elm_haifisch$App$eventToCmd = function (event) {
	var _p0 = event;
	switch (_p0.ctor) {
		case 'ShipExplodes':
			return _xarvh$elm_haifisch$SoundPort$playSound('explosion');
		case 'ShipFires':
			return _xarvh$elm_haifisch$SoundPort$playSound('fire');
		case 'ShipActivates':
			return _xarvh$elm_haifisch$SoundPort$playSound('spawnEnd');
		case 'ShipAppears':
			return _elm_lang$core$Platform_Cmd$none;
		default:
			return _elm_lang$core$Platform_Cmd$none;
	}
};
var _xarvh$elm_haifisch$App$resizeWindow = F2(
	function (sizeInPixels, model) {
		var internalCoordinatesHeight = _xarvh$elm_haifisch$Game$worldRadius * 2.1;
		var internalCoordinatesWidth = (_elm_lang$core$Basics$toFloat(sizeInPixels.width) * internalCoordinatesHeight) / _elm_lang$core$Basics$toFloat(sizeInPixels.height);
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				windowSizeInPixels: sizeInPixels,
				windowSizeInGameCoordinates: A2(_elm_community$linear_algebra$Math_Vector2$vec2, internalCoordinatesWidth, internalCoordinatesHeight)
			});
	});
var _xarvh$elm_haifisch$App$noCmd = function (model) {
	return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
};
var _xarvh$elm_haifisch$App$Config = F2(
	function (a, b) {
		return {maybeInputConfig: a, gamepadDatabase: b};
	});
var _xarvh$elm_haifisch$App$Model = F6(
	function (a, b, c, d, e, f) {
		return {bots: a, controllersAndPlayerIds: b, input: c, game: d, windowSizeInPixels: e, windowSizeInGameCoordinates: f};
	});
var _xarvh$elm_haifisch$App$ControllerBot = function (a) {
	return {ctor: 'ControllerBot', _0: a};
};
var _xarvh$elm_haifisch$App$ControllerHuman = function (a) {
	return {ctor: 'ControllerHuman', _0: a};
};
var _xarvh$elm_haifisch$App$getPlayerMouseAim = F3(
	function (model, inputSource, mousePositionInWindowCoordinates) {
		var findShipOwnedByPlayerId = function (playerId) {
			return A2(
				_elm_community$dict_extra$Dict_Extra$find,
				F2(
					function (shipId, ship) {
						return _elm_lang$core$Native_Utils.eq(ship.playerId, playerId);
					}),
				model.game.shipsById);
		};
		var findPlayerIdThatHasController = function (controller) {
			return A2(
				_elm_lang$core$Maybe$map,
				_elm_lang$core$Tuple$second,
				A2(
					_elm_community$list_extra$List_Extra$find,
					function (_p1) {
						var _p2 = _p1;
						return _elm_lang$core$Native_Utils.eq(_p2._0, controller);
					},
					model.controllersAndPlayerIds));
		};
		var maybeControlledShip = A2(
			_elm_lang$core$Maybe$andThen,
			findShipOwnedByPlayerId,
			findPlayerIdThatHasController(
				_xarvh$elm_haifisch$App$ControllerHuman(inputSource)));
		var _p3 = maybeControlledShip;
		if (_p3.ctor === 'Nothing') {
			return A2(_elm_community$linear_algebra$Math_Vector2$vec2, 0, 1);
		} else {
			var _p4 = _elm_community$linear_algebra$Math_Vector2$toTuple(model.windowSizeInGameCoordinates);
			var gameW = _p4._0;
			var gameH = _p4._1;
			var pixelH = _elm_lang$core$Basics$toFloat(model.windowSizeInPixels.height);
			var pixelW = _elm_lang$core$Basics$toFloat(model.windowSizeInPixels.width);
			var mousePixelY = _elm_lang$core$Basics$toFloat(mousePositionInWindowCoordinates.y);
			var mouseGameY = (gameH * (((pixelH / 2) - mousePixelY) - 1)) / pixelH;
			var mousePixelX = _elm_lang$core$Basics$toFloat(mousePositionInWindowCoordinates.x);
			var mouseGameX = (gameW * (mousePixelX - (pixelW / 2))) / pixelW;
			var mouseGamePos = A2(_elm_community$linear_algebra$Math_Vector2$vec2, mouseGameX, mouseGameY);
			return _elm_community$linear_algebra$Math_Vector2$normalize(
				A2(_elm_community$linear_algebra$Math_Vector2$sub, mouseGamePos, _p3._0._1.position));
		}
	});
var _xarvh$elm_haifisch$App$resolveMouseAim = F3(
	function (model, inputSource, rawInputState) {
		var finalAim = function () {
			var _p5 = rawInputState.finalAim;
			if (_p5.ctor === 'Direction') {
				return _p5._0;
			} else {
				return A3(_xarvh$elm_haifisch$App$getPlayerMouseAim, model, inputSource, _p5._0);
			}
		}();
		return {finalAim: finalAim, fire: rawInputState.fire, move: rawInputState.move};
	});
var _xarvh$elm_haifisch$App$manageInput = F3(
	function (config, blob, model) {
		var addPlayer = F2(
			function (controller, _p6) {
				var _p7 = _p6;
				var _p8 = _xarvh$elm_haifisch$Game$addPlayer(_p7._0);
				var newGame = _p8._0;
				var player = _p8._1;
				return {
					ctor: '_Tuple2',
					_0: newGame,
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: controller, _1: player.id},
						_1: _p7._1
					}
				};
			});
		var controllersWithAPlayer = A2(_elm_lang$core$List$map, _elm_lang$core$Tuple$first, model.controllersAndPlayerIds);
		var findPlayerId = function (_p9) {
			var _p10 = _p9;
			return A2(
				_elm_lang$core$Maybe$map,
				function (_p11) {
					var _p12 = _p11;
					return {ctor: '_Tuple2', _0: _p12._1, _1: _p10._1};
				},
				A2(
					_elm_community$list_extra$List_Extra$find,
					function (_p13) {
						var _p14 = _p13;
						return _elm_lang$core$Native_Utils.eq(_p14._0, _p10._0);
					},
					model.controllersAndPlayerIds));
		};
		var botsAndStates = A2(
			_elm_lang$core$List$map,
			_xarvh$elm_haifisch$Bots$think(model.game),
			model.bots);
		var botControllersAndStates = A2(
			_elm_lang$core$List$map,
			_elm_lang$core$Tuple$mapFirst(
				function (_p15) {
					return _xarvh$elm_haifisch$App$ControllerBot(
						function (_) {
							return _.id;
						}(_p15));
				}),
			botsAndStates);
		var inputSourcesAndRawStates = A3(
			_xarvh$elm_haifisch$Input$sourcesAndStates,
			config.maybeInputConfig,
			A2(_xarvh$elm_gamepad$Gamepad$getGamepads, config.gamepadDatabase, blob),
			model.input);
		var humanControllersAndStates = A2(
			_elm_lang$core$List$map,
			function (_p16) {
				var _p17 = _p16;
				var _p18 = _p17._0;
				return {
					ctor: '_Tuple2',
					_0: _xarvh$elm_haifisch$App$ControllerHuman(_p18),
					_1: A3(_xarvh$elm_haifisch$App$resolveMouseAim, model, _p18, _p17._1)
				};
			},
			inputSourcesAndRawStates);
		var controllersAndStates = A2(_elm_lang$core$Basics_ops['++'], humanControllersAndStates, botControllersAndStates);
		var stateByPlayerId = _elm_lang$core$Dict$fromList(
			A2(_elm_lang$core$List$filterMap, findPlayerId, controllersAndStates));
		var controllersWithoutAPlayer = A2(
			_elm_lang$core$List$filter,
			function (c) {
				return !A2(_elm_lang$core$List$member, c, controllersWithAPlayer);
			},
			A2(_elm_lang$core$List$map, _elm_lang$core$Tuple$first, controllersAndStates));
		var _p19 = A3(
			_elm_lang$core$List$foldl,
			addPlayer,
			{ctor: '_Tuple2', _0: model.game, _1: model.controllersAndPlayerIds},
			controllersWithoutAPlayer);
		var game = _p19._0;
		var controllersAndPlayerIds = _p19._1;
		return {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Native_Utils.update(
				model,
				{
					game: game,
					controllersAndPlayerIds: controllersAndPlayerIds,
					bots: A2(_elm_lang$core$List$map, _elm_lang$core$Tuple$first, botsAndStates)
				}),
			_1: stateByPlayerId
		};
	});
var _xarvh$elm_haifisch$App$updateAnimationFrame = F4(
	function (config, dt, blob, oldModel) {
		var _p20 = A3(_xarvh$elm_haifisch$App$manageInput, config, blob, oldModel);
		var model = _p20._0;
		var inputStatesByPlayerId = _p20._1;
		var _p21 = A3(_xarvh$elm_haifisch$Game$tick, inputStatesByPlayerId, dt, model.game);
		var game = _p21._0;
		var events = _p21._1;
		var cmds = A2(_elm_lang$core$List$map, _xarvh$elm_haifisch$App$eventToCmd, events);
		return {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Native_Utils.update(
				model,
				{game: game}),
			_1: _elm_lang$core$Platform_Cmd$batch(cmds)
		};
	});
var _xarvh$elm_haifisch$App$update = F4(
	function (isPaused, config, msg, model) {
		var _p22 = msg;
		switch (_p22.ctor) {
			case 'OnAnimationFrame':
				return isPaused ? _xarvh$elm_haifisch$App$noCmd(model) : A4(_xarvh$elm_haifisch$App$updateAnimationFrame, config, _p22._0._0, _p22._0._1, model);
			case 'OnInputMsg':
				return _xarvh$elm_haifisch$App$noCmd(
					_elm_lang$core$Native_Utils.update(
						model,
						{
							input: A2(_xarvh$elm_haifisch$Input$update, _p22._0, model.input)
						}));
			default:
				return _xarvh$elm_haifisch$App$noCmd(
					A2(_xarvh$elm_haifisch$App$resizeWindow, _p22._0, model));
		}
	});
var _xarvh$elm_haifisch$App$OnWindowResizes = function (a) {
	return {ctor: 'OnWindowResizes', _0: a};
};
var _xarvh$elm_haifisch$App$init = function (dateNow) {
	var game = _xarvh$elm_haifisch$Game$init(
		_elm_lang$core$Random$initialSeed(dateNow));
	var bots = A2(
		_xarvh$elm_haifisch$Bots$addBot,
		_xarvh$elm_haifisch$Bots$fullRandomBot(game),
		{ctor: '[]'});
	return {
		ctor: '_Tuple2',
		_0: {
			bots: bots,
			controllersAndPlayerIds: {ctor: '[]'},
			input: _xarvh$elm_haifisch$Input$init,
			game: game,
			windowSizeInPixels: A2(_elm_lang$window$Window$Size, 1, 1),
			windowSizeInGameCoordinates: A2(_elm_community$linear_algebra$Math_Vector2$vec2, 1, 1)
		},
		_1: A2(_elm_lang$core$Task$perform, _xarvh$elm_haifisch$App$OnWindowResizes, _elm_lang$window$Window$size)
	};
};
var _xarvh$elm_haifisch$App$OnInputMsg = function (a) {
	return {ctor: 'OnInputMsg', _0: a};
};
var _xarvh$elm_haifisch$App$OnAnimationFrame = function (a) {
	return {ctor: 'OnAnimationFrame', _0: a};
};
var _xarvh$elm_haifisch$App$subscriptions = function (model) {
	return _elm_lang$core$Platform_Sub$batch(
		{
			ctor: '::',
			_0: _elm_lang$window$Window$resizes(_xarvh$elm_haifisch$App$OnWindowResizes),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$core$Platform_Sub$map,
					_xarvh$elm_haifisch$App$OnInputMsg,
					_xarvh$elm_haifisch$Input$subscriptions(model.input)),
				_1: {
					ctor: '::',
					_0: _xarvh$elm_haifisch$GamepadPort$gamepad(_xarvh$elm_haifisch$App$OnAnimationFrame),
					_1: {ctor: '[]'}
				}
			}
		});
};

var _xarvh$elm_haifisch$Config$noCmd = function (model) {
	return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
};
var _xarvh$elm_haifisch$Config$gamepadControls = {
	ctor: '::',
	_0: {ctor: '_Tuple2', _0: _xarvh$elm_gamepad$Gamepad$A, _1: 'Fire'},
	_1: {
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: _xarvh$elm_gamepad$Gamepad$B, _1: 'Fire (different button)'},
		_1: {
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: _xarvh$elm_gamepad$Gamepad$LeftLeft, _1: 'Move left'},
			_1: {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: _xarvh$elm_gamepad$Gamepad$LeftRight, _1: 'Move right'},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: _xarvh$elm_gamepad$Gamepad$LeftUp, _1: 'Move up'},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: _xarvh$elm_gamepad$Gamepad$LeftDown, _1: 'Move down'},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: _xarvh$elm_gamepad$Gamepad$RightLeft, _1: 'Aim left'},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: _xarvh$elm_gamepad$Gamepad$RightRight, _1: 'Aim right'},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: _xarvh$elm_gamepad$Gamepad$RightUp, _1: 'Aim up'},
									_1: {
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: _xarvh$elm_gamepad$Gamepad$RightDown, _1: 'Aim down'},
										_1: {ctor: '[]'}
									}
								}
							}
						}
					}
				}
			}
		}
	}
};
var _xarvh$elm_haifisch$Config$Flags = F3(
	function (a, b, c) {
		return {gamepadDatabaseAsString: a, gamepadDatabaseKey: b, dateNow: c};
	});
var _xarvh$elm_haifisch$Config$Model = F7(
	function (a, b, c, d, e, f, g) {
		return {app: a, gamepadDatabase: b, gamepadDatabaseKey: c, hasGamepads: d, hasKnownGamepads: e, maybeInputConfig: f, maybeModal: g};
	});
var _xarvh$elm_haifisch$Config$Remapping = function (a) {
	return {ctor: 'Remapping', _0: a};
};
var _xarvh$elm_haifisch$Config$Message = function (a) {
	return {ctor: 'Message', _0: a};
};
var _xarvh$elm_haifisch$Config$updateRemap = F3(
	function (remapMsg, remapModel, model) {
		var _p0 = A2(_xarvh$elm_gamepad$Gamepad_Remap$update, remapMsg, remapModel);
		switch (_p0.ctor) {
			case 'StillOpen':
				return _xarvh$elm_haifisch$Config$noCmd(
					_elm_lang$core$Native_Utils.update(
						model,
						{
							maybeModal: _elm_lang$core$Maybe$Just(
								_xarvh$elm_haifisch$Config$Remapping(_p0._0))
						}));
			case 'Error':
				return _xarvh$elm_haifisch$Config$noCmd(
					_elm_lang$core$Native_Utils.update(
						model,
						{
							maybeModal: _elm_lang$core$Maybe$Just(
								_xarvh$elm_haifisch$Config$Message(_p0._0))
						}));
			default:
				var gamepadDatabase = _p0._0(model.gamepadDatabase);
				var cmd = A2(
					_xarvh$elm_haifisch$LocalStoragePort$set,
					model.gamepadDatabaseKey,
					_xarvh$elm_gamepad$Gamepad$databaseToString(gamepadDatabase));
				var newModel = _elm_lang$core$Native_Utils.update(
					model,
					{
						maybeModal: _elm_lang$core$Maybe$Just(
							_xarvh$elm_haifisch$Config$Message('Gamepad configured!')),
						gamepadDatabase: gamepadDatabase
					});
				return {ctor: '_Tuple2', _0: newModel, _1: cmd};
		}
	});
var _xarvh$elm_haifisch$Config$Main = {ctor: 'Main'};
var _xarvh$elm_haifisch$Config$OnRemapMsg = function (a) {
	return {ctor: 'OnRemapMsg', _0: a};
};
var _xarvh$elm_haifisch$Config$OnRemapButton = {ctor: 'OnRemapButton'};
var _xarvh$elm_haifisch$Config$OnInputConfig = function (a) {
	return {ctor: 'OnInputConfig', _0: a};
};
var _xarvh$elm_haifisch$Config$viewInputConfig = F2(
	function (hasKnownGamepads, maybeInputConfig) {
		return A2(
			_elm_lang$html$Html$div,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$div,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text('Use keyboard?'),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$select,
						{
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html_Events$on,
								'change',
								A2(_elm_lang$core$Json_Decode$map, _xarvh$elm_haifisch$Config$OnInputConfig, _elm_lang$html$Html_Events$targetValue)),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$disabled(!hasKnownGamepads),
								_1: {ctor: '[]'}
							}
						},
						{
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$option,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$value(''),
									_1: {
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$selected(
											_elm_lang$core$Native_Utils.eq(maybeInputConfig, _elm_lang$core$Maybe$Nothing)),
										_1: {ctor: '[]'}
									}
								},
								{
									ctor: '::',
									_0: _elm_lang$html$Html$text('Guess'),
									_1: {ctor: '[]'}
								}),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_lang$html$Html$option,
									{
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$value('key'),
										_1: {
											ctor: '::',
											_0: _elm_lang$html$Html_Attributes$selected(
												_elm_lang$core$Native_Utils.eq(
													maybeInputConfig,
													_elm_lang$core$Maybe$Just(_xarvh$elm_haifisch$Input$OnePlayerUsesKeyboardAndMouse))),
											_1: {ctor: '[]'}
										}
									},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text('A player uses the keyboard'),
										_1: {ctor: '[]'}
									}),
								_1: {
									ctor: '::',
									_0: A2(
										_elm_lang$html$Html$option,
										{
											ctor: '::',
											_0: _elm_lang$html$Html_Attributes$value('pad'),
											_1: {
												ctor: '::',
												_0: _elm_lang$html$Html_Attributes$selected(
													_elm_lang$core$Native_Utils.eq(
														maybeInputConfig,
														_elm_lang$core$Maybe$Just(_xarvh$elm_haifisch$Input$AllPlayersUseGamepads))),
												_1: {ctor: '[]'}
											}
										},
										{
											ctor: '::',
											_0: _elm_lang$html$Html$text('Everyone uses only gamepads'),
											_1: {ctor: '[]'}
										}),
									_1: {ctor: '[]'}
								}
							}
						}),
					_1: {ctor: '[]'}
				}
			});
	});
var _xarvh$elm_haifisch$Config$viewConfig = function (model) {
	return A2(
		_elm_lang$html$Html$div,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$div,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('ConfigModal-Item'),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text('Press Esc to to toggle Menu'),
					_1: {ctor: '[]'}
				}),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$div,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('ConfigModal-Item'),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: A2(_xarvh$elm_haifisch$Config$viewInputConfig, model.hasKnownGamepads, model.maybeInputConfig),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$div,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('ConfigModal-Item'),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$button,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$disabled(!model.hasGamepads),
									_1: {
										ctor: '::',
										_0: _elm_lang$html$Html_Events$onClick(_xarvh$elm_haifisch$Config$OnRemapButton),
										_1: {ctor: '[]'}
									}
								},
								{
									ctor: '::',
									_0: _elm_lang$html$Html$text('Remap gamepads'),
									_1: {ctor: '[]'}
								}),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _xarvh$elm_haifisch$Config$OnKey = function (a) {
	return {ctor: 'OnKey', _0: a};
};
var _xarvh$elm_haifisch$Config$OnGamepad = function (a) {
	return {ctor: 'OnGamepad', _0: a};
};
var _xarvh$elm_haifisch$Config$OnAppMsg = function (a) {
	return {ctor: 'OnAppMsg', _0: a};
};
var _xarvh$elm_haifisch$Config$init = function (flags) {
	var gamepadDatabase = A2(
		_elm_lang$core$Result$withDefault,
		_xarvh$elm_gamepad$Gamepad$emptyDatabase,
		_xarvh$elm_gamepad$Gamepad$databaseFromString(flags.gamepadDatabaseAsString));
	var _p1 = _xarvh$elm_haifisch$App$init(flags.dateNow);
	var app = _p1._0;
	var appCmd = _p1._1;
	var model = {
		app: app,
		gamepadDatabase: gamepadDatabase,
		gamepadDatabaseKey: flags.gamepadDatabaseKey,
		hasGamepads: false,
		hasKnownGamepads: false,
		maybeInputConfig: _elm_lang$core$Maybe$Nothing,
		maybeModal: _elm_lang$core$Maybe$Just(_xarvh$elm_haifisch$Config$Main)
	};
	var cmd = A2(_elm_lang$core$Platform_Cmd$map, _xarvh$elm_haifisch$Config$OnAppMsg, appCmd);
	return {ctor: '_Tuple2', _0: model, _1: cmd};
};
var _xarvh$elm_haifisch$Config$update = F2(
	function (msg, model) {
		var _p2 = msg;
		switch (_p2.ctor) {
			case 'OnAppMsg':
				return A2(
					_elm_lang$core$Tuple$mapSecond,
					_elm_lang$core$Platform_Cmd$map(_xarvh$elm_haifisch$Config$OnAppMsg),
					A2(
						_elm_lang$core$Tuple$mapFirst,
						function (app) {
							return _elm_lang$core$Native_Utils.update(
								model,
								{app: app});
						},
						A4(
							_xarvh$elm_haifisch$App$update,
							!_elm_lang$core$Native_Utils.eq(model.maybeModal, _elm_lang$core$Maybe$Nothing),
							{maybeInputConfig: model.maybeInputConfig, gamepadDatabase: model.gamepadDatabase},
							_p2._0,
							model.app)));
			case 'OnGamepad':
				var _p3 = _p2._0._1;
				var allGamepads = _elm_lang$core$List$length(
					_xarvh$elm_gamepad$Gamepad$getAllGamepadsAsUnknown(_p3));
				var knownGamepads = _elm_lang$core$List$length(
					A2(_xarvh$elm_gamepad$Gamepad$getGamepads, model.gamepadDatabase, _p3));
				return _xarvh$elm_haifisch$Config$noCmd(
					_elm_lang$core$Native_Utils.update(
						model,
						{
							hasGamepads: _elm_lang$core$Native_Utils.cmp(allGamepads, 0) > 0,
							hasKnownGamepads: _elm_lang$core$Native_Utils.cmp(knownGamepads, 0) > 0
						}));
			case 'OnKey':
				var _p4 = _p2._0;
				if (_p4 === 27) {
					return _xarvh$elm_haifisch$Config$noCmd(
						_elm_lang$core$Native_Utils.update(
							model,
							{
								maybeModal: function () {
									var _p5 = model.maybeModal;
									if (_p5.ctor === 'Nothing') {
										return _elm_lang$core$Maybe$Just(_xarvh$elm_haifisch$Config$Main);
									} else {
										return _elm_lang$core$Maybe$Nothing;
									}
								}()
							}));
				} else {
					return _xarvh$elm_haifisch$Config$noCmd(model);
				}
			case 'OnInputConfig':
				return _xarvh$elm_haifisch$Config$noCmd(
					_elm_lang$core$Native_Utils.update(
						model,
						{
							maybeInputConfig: function () {
								var _p6 = _p2._0;
								switch (_p6) {
									case 'key':
										return _elm_lang$core$Maybe$Just(_xarvh$elm_haifisch$Input$OnePlayerUsesKeyboardAndMouse);
									case 'pad':
										return _elm_lang$core$Maybe$Just(_xarvh$elm_haifisch$Input$AllPlayersUseGamepads);
									default:
										return _elm_lang$core$Maybe$Nothing;
								}
							}()
						}));
			case 'OnRemapButton':
				return _xarvh$elm_haifisch$Config$noCmd(
					_elm_lang$core$Native_Utils.update(
						model,
						{
							maybeModal: _elm_lang$core$Maybe$Just(
								_xarvh$elm_haifisch$Config$Remapping(
									A2(_xarvh$elm_gamepad$Gamepad_Remap$init, 0, _xarvh$elm_haifisch$Config$gamepadControls)))
						}));
			default:
				var _p7 = model.maybeModal;
				if ((_p7.ctor === 'Just') && (_p7._0.ctor === 'Remapping')) {
					return A3(_xarvh$elm_haifisch$Config$updateRemap, _p2._0, _p7._0._0, model);
				} else {
					return _xarvh$elm_haifisch$Config$noCmd(model);
				}
		}
	});
var _xarvh$elm_haifisch$Config$view = function (model) {
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('root'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$map,
				_xarvh$elm_haifisch$Config$OnAppMsg,
				_xarvh$elm_haifisch$App$view(model.app)),
			_1: {
				ctor: '::',
				_0: function () {
					var _p8 = model.maybeModal;
					if (_p8.ctor === 'Nothing') {
						return _elm_lang$html$Html$text('');
					} else {
						return A2(
							_elm_lang$html$Html$div,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('ConfigModal-Container'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: A2(
									_elm_lang$html$Html$div,
									{
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$class('ConfigModal-Content'),
										_1: {ctor: '[]'}
									},
									{
										ctor: '::',
										_0: function () {
											var _p9 = _p8._0;
											switch (_p9.ctor) {
												case 'Main':
													return _xarvh$elm_haifisch$Config$viewConfig(model);
												case 'Message':
													return _elm_lang$html$Html$text(_p9._0);
												default:
													return _elm_lang$html$Html$text(
														_xarvh$elm_gamepad$Gamepad_Remap$view(_p9._0));
											}
										}(),
										_1: {ctor: '[]'}
									}),
								_1: {ctor: '[]'}
							});
					}
				}(),
				_1: {ctor: '[]'}
			}
		});
};
var _xarvh$elm_haifisch$Config$subscriptions = function (model) {
	return _elm_lang$core$Platform_Sub$batch(
		{
			ctor: '::',
			_0: _elm_lang$keyboard$Keyboard$ups(_xarvh$elm_haifisch$Config$OnKey),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$core$Platform_Sub$map,
					_xarvh$elm_haifisch$Config$OnAppMsg,
					_xarvh$elm_haifisch$App$subscriptions(model.app)),
				_1: {
					ctor: '::',
					_0: function () {
						var _p10 = model.maybeModal;
						if (_p10.ctor === 'Nothing') {
							return _elm_lang$core$Platform_Sub$none;
						} else {
							switch (_p10._0.ctor) {
								case 'Main':
									return _xarvh$elm_haifisch$GamepadPort$gamepad(_xarvh$elm_haifisch$Config$OnGamepad);
								case 'Remapping':
									return A2(
										_elm_lang$core$Platform_Sub$map,
										_xarvh$elm_haifisch$Config$OnRemapMsg,
										_xarvh$elm_gamepad$Gamepad_Remap$subscriptions(_xarvh$elm_haifisch$GamepadPort$gamepad));
								default:
									return _elm_lang$core$Platform_Sub$none;
							}
						}
					}(),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _xarvh$elm_haifisch$Config$programWithFlags = {init: _xarvh$elm_haifisch$Config$init, update: _xarvh$elm_haifisch$Config$update, view: _xarvh$elm_haifisch$Config$view, subscriptions: _xarvh$elm_haifisch$Config$subscriptions};

var _xarvh$elm_haifisch$Main$main = _elm_lang$html$Html$programWithFlags(_xarvh$elm_haifisch$Config$programWithFlags)(
	A2(
		_elm_lang$core$Json_Decode$andThen,
		function (dateNow) {
			return A2(
				_elm_lang$core$Json_Decode$andThen,
				function (gamepadDatabaseAsString) {
					return A2(
						_elm_lang$core$Json_Decode$andThen,
						function (gamepadDatabaseKey) {
							return _elm_lang$core$Json_Decode$succeed(
								{dateNow: dateNow, gamepadDatabaseAsString: gamepadDatabaseAsString, gamepadDatabaseKey: gamepadDatabaseKey});
						},
						A2(_elm_lang$core$Json_Decode$field, 'gamepadDatabaseKey', _elm_lang$core$Json_Decode$string));
				},
				A2(_elm_lang$core$Json_Decode$field, 'gamepadDatabaseAsString', _elm_lang$core$Json_Decode$string));
		},
		A2(_elm_lang$core$Json_Decode$field, 'dateNow', _elm_lang$core$Json_Decode$int)));

var Elm = {};
Elm['Main'] = Elm['Main'] || {};
if (typeof _xarvh$elm_haifisch$Main$main !== 'undefined') {
    _xarvh$elm_haifisch$Main$main(Elm['Main'], 'Main', undefined);
}

if (typeof define === "function" && define['amd'])
{
  define([], function() { return Elm; });
  return;
}

if (typeof module === "object")
{
  module['exports'] = Elm;
  return;
}

var globalElm = this['Elm'];
if (typeof globalElm === "undefined")
{
  this['Elm'] = Elm;
  return;
}

for (var publicModule in Elm)
{
  if (publicModule in globalElm)
  {
    throw new Error('There are two Elm modules called `' + publicModule + '` on this page! Rename one of them.');
  }
  globalElm[publicModule] = Elm[publicModule];
}

}).call(this);

