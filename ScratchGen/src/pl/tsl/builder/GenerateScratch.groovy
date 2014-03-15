// This script is used to generate Scratch json scripts to easier manage bigger projects

package pl.tsl.builder

BigDecimal.metaClass.plus = {Expression x -> new Number(delegate) + x}
BigDecimal.metaClass.minus = {Expression x -> new Number(delegate) - x}
BigDecimal.metaClass.multiply = {Expression x -> new Number(delegate) * x}
BigDecimal.metaClass.div = {Expression x -> new Number(delegate) / x}
BigDecimal.metaClass.mod = {Expression x -> new Number(delegate) % x}
BigDecimal.metaClass.or = {Expression x -> new Number(delegate) | x}
BigDecimal.metaClass.and = {Expression x -> new Number(delegate) & x}

abstract class Expression {
	String name
	
	Expression plus(Expression op) { new Function('"+"',[this,op]) }
	Expression minus(Expression op) { new Function('"-"',[this,op]) }
	Expression multiply(Expression op) { new Function('"*"',[this,op]) }
	Expression div(Expression op) { new Function('"\\/"',[this,op]) }
	Expression mod(Expression op) {	new Function('"%"',[this,op]) }
	
	Expression or(Expression op) { new Function('"|"',[this,op]) }
	Expression and(Expression op) { new Function('"&"',[this,op]) }
		
	Expression plus(BigDecimal op) { this + new Number(op) }
	Expression minus(BigDecimal op) { this - new Number(op) }
	Expression multiply(BigDecimal op) { this * new Number(op) }
	Expression div(BigDecimal op) { this / new Number(op) }
	Expression mod(BigDecimal op) {	this % new Number(op) }
	
	Expression or(BigDecimal op) { this | new Number(op) }
	Expression and(BigDecimal op) { this & new Number(op) }
	
	Expression negative() { new Number(-1) * this }
	Expression bitwiseNegate() { new ComputeFunction('"not"',this) }
	
	public Expression(String n) { name = n }
}

class Number extends Expression {
	BigDecimal val
	public Number(BigDecimal v) { super("n"); val = v }
	public String toString() { return val.toString() }
}

class Function extends Expression {
	def list
	public Function(String n, List l) {
		super(n)
		list = l
	}
	public String toString() { return "[" + name + ", " + list.collect{ it.toString() }.join(', ') + "]" }
}

class ComputeFunction extends Expression {
	Expression op
	public ComputeFunction(String n, Expression exp) {
		super(n)
		op = exp
	}
	public ComputeFunction(String n,BigDecimal o) { this(n, new Number(o)) }
	
	public String toString() { return "[" + '"computeFunction:of:", ' + name + ", "  + op.toString() + "]"  }
}

class Command extends Expression {
	int nr = 0
	Expression v
	List opt1, opt2
	public Command(String n, Expression o0) { super(n); nr = 1; v = o0 }
	public Command(String n, Expression o0, List o1) { super(n); nr = 2; v = o0; opt1 = o1 }
	public Command(String n, Expression o0, List o1, List o2) { super(n); nr = 3; v = o0; opt1 = o1; opt2 = o2 }
	public String toString() {
		def b = "[" << name << ",\n"
		b << v.toString()
		switch(nr) {
			case 2: b << ", " << opt1.toString() << "\n"; break;
			case 3: b << ", " << opt1.toString() << ",\n" << opt2.toString() << "\n" ; break
		}
		b << "]\n"
		return b.toString()
	}
}

// Variable handling
class Vars {
	def varSet = [] as Set
	public Vars leftShift(String n) { varSet << n ; return this }
	public Vars leftShift(Expression e) { varSet << e.name ; return this }
	public String toString() {
		def b = '"variables": [' << "\n"
		b << varSet.collect {
			"\t{\n" +
			"\t\t" + '"name": "'  + it + '",' + "\n" +
			"\t\t" + '"value": 1,' + "\n" +
			"\t\t" + '"isPersistent": false' + "\n" +
			"\t}"
		}.join(",\n")
		b << "],\n"
		return b.toString()
	}
}

class VariableS extends ComputeFunction {
	public VariableS(String n, Expression par) { super(n,par) }
	public VariableS(String n, BigDecimal par) { super(n,par) }
	public String toString() { return '["setVar:to:", "' + name + '", ' + op.toString() + ']' }
}

class VariableG extends Expression {
	static def varSet = new Vars()
	def change = false
	Expression changeval
	public VariableG(String n) { super(n); varSet << n }
	
	private changeme(Expression e) { change = true; changeval = e }
	private changeme(BigDecimal n) { changeme(new Number(n)) }
	
	VariableG next() { changeme(1.0); this }
	VariableG prev() { changeme(-1.0); this }
	
	VariableS leftShift(Expression ex) {
		return new VariableS(name,ex)
	}
	
	public String toString() { 
		if(change) 
			return '["changeVar:by:", "' + name + '", ' + changeval.toString() + "]"
		else
			return '["readVariable", "' + name + '"]'
	}
}

class Var {
	Object getProperty(String property){
		return new VariableG(property)
	}
}

def V = new Var()

// Script handling
def position = 0
def Script = { blocks ->
	position += 100
	"[" + position + "," + position + ", [" + blocks.collect { it.toString() }.join(",\n") + "]]\n"
} 

def Scripts =  { scripts ->
	println '"scripts": [' + scripts.join(",\n") + "],\n"
	println VariableG.varSet.toString()
}

// Aliases
def N = { x -> new Number(x) }
def LT = { l, r -> new Function('"<"',[l,r]) }
def GT = { l, r -> new Function('">"',[l,r]) }
def EQ = { l, r -> new Function('"="',[l,r]) }

def Abs = { par -> new ComputeFunction('"abs"',par) }
def Floor = { par -> new ComputeFunction('"floor"',par) }
def Ceiling = { par -> new ComputeFunction('"ceiling"',par) }
def Sqrt = { par -> new ComputeFunction('"sqrt"',par) }
def Sin = { par -> new ComputeFunction('"sin"',par) }
def Cos = { par -> new ComputeFunction('"cos"',par) }
def Tan = { par -> new ComputeFunction('"tan"',par) }
def ASin = { par -> new ComputeFunction('"asin"',par) }
def ACos = { par -> new ComputeFunction('"acos"',par) }
def ATan = { par -> new ComputeFunction('"atan"',par) }
def Ln = { par -> new ComputeFunction('"ln"',par) }
def Log = { par -> new ComputeFunction('"log"',par) }
def Exp = { par -> new ComputeFunction('"e ^"',par) }
def Pow10 = { par -> new ComputeFunction('"10 ^"',par) }
def Round = { par -> new Function('"rounded"', [par]) }
def Rnd = { from, to -> new Function('"randomFrom:to:"',[from,to]) }

def IfElse = { cond,op1,op2 -> new Command('"doIfElse"',cond,op1,op2) }
def If = { cond,op1 -> new Command('"doIfElse"',cond,op1) }

// Math aliases
def M_E = 		N(2.7182818284590452354)
def M_LOG2E = 	N(1.4426950408889634074)
def M_LOG10E = 	N(0.43429448190325182765)
def M_LN2 =		N(0.69314718055994530942)
def M_LN10 =	N(2.30258509299404568402)
def M_PI =		N(3.14159265358979323846)
def M_PI_2 =	N(1.57079632679489661923)
def M_PI_4 =	N(0.78539816339744830962)
def M_1_PI =	N(0.31830988618379067154)
def M_2_PI =	N(0.63661977236758134308)
def M_2_SQRTPI =N(1.12837916709551257390)
def M_SQRT2 =	N(1.41421356237309504880)
def M_SQRT1_2 =	N(0.7071067811865475244)
def M_2PI =     N(6.283185307179586476925286766559)
def M_3PI_4 =   N(2.3561944901923449288469825374596)
def M_SQRT3 =   N(1.7320508075688772935274463415059)
def M_SQRT3_2 = N(0.86602540378443864676372317075249)
def M_SQRT5 =   N(2.2360679774997896964091736687313)
def M_PHI =     N(1.61803398874989484820458683436563) // golden ratio
def M_1_2PI =   N(0.1591549430918953357688837633725)
def IM =		N(2147483647.0)
def AM = 		N(0.000000000465661287524579692410575)
def R2D	=		N(57.295779513082320876798154814105) // rad to deg conversion
def D2R = 		N(0.01745329251994329576923690768489) // deg to rad conversion


// Your script generation goes here

Scripts([
	
	Script([
		
		V.blah << V.parameter, 
		V.blah <<  V.blah,
		
		IfElse(
			EQ(3.0,5.0) & LT(V.par1,33.3), 
			[V.blah << Sin(V.par1*V.par2)],
			[V.ttt << Cos(V.par1/V.par2),
				V.par1 << V.par2,
				V.par2 << V.par1
			]
		)

	]),
	
	Script([
		
		V.par1 << M_PI,
		V.par2 << Rnd(0,1000.0) / 1000.0
		
	])
	
])
