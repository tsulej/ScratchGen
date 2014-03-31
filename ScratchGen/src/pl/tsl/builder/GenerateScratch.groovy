// This script is used to generate Scratch json scripts to easier managing bigger projects

package pl.tsl.builder

class Expression {
	String name
	String nameclean
	
	Expression plus(Expression op) { new Function('+',[this,op]) }
	Expression minus(Expression op) { new Function('-',[this,op]) }
	Expression multiply(Expression op) { new Function('*',[this,op]) }
	Expression div(Expression op) { new Function("\\/",[this,op]) }
	Expression mod(Expression op) {	new Function('%',[this,op]) }
	
	Expression or(Expression op) { new Function('|',[this,op]) }
	Expression and(Expression op) { new Function('&',[this,op]) }
		
	Expression plus(BigDecimal op) { this + new Number(op) }
	Expression minus(BigDecimal op) { this - new Number(op) }
	Expression multiply(BigDecimal op) { this * new Number(op) }
	Expression div(BigDecimal op) { this / new Number(op) }
	Expression mod(BigDecimal op) {	this % new Number(op) }
	
	Expression or(BigDecimal op) { this | new Number(op) }
	Expression and(BigDecimal op) { this & new Number(op) }
	
	Expression negative() { new Number(-1.0) * this }
	Expression bitwiseNegate() { new Function('not',[this]) }
	
	public Expression(String n) { name = '"' + n + '"'; nameclean = n }
	public String toString() { return name } 
	public Expression noquotation() { name = nameclean; return this }
	
	public def eval = { return nameclean }
}

class Number extends Expression {
	BigDecimal val
	public Number(BigDecimal v) { super("n"); val = v }
	public String toString() { return val.toString() }
	
	Number plus(Number op) { new Number( this.val + op.val) }
	Number minus(Number op) { new Number( this.val - op.val) }
	Number multiply(Number op) { new Number( this.val * op.val) }
	Number div(Number op) { new Number( this.val / op.val) }
	Number mod(Number op) {	new Number( this.val % op.val) }
	
	public def eval = { return val }
}

class Function extends Expression {
	def list
	static final Random rnd = new Random()
	public static defbodies =[:]
	
	public Function(String n, List l) {
		super(n)
		list = l.collect { if(it instanceof Expression) it else new Number(it) }
	}
	public String toString() { return "[" + name + ", " + list.collect{ it.toString() }.join(', ') + "]" }
	
	public def eval = {
		switch(nameclean) {
			case '+': return list[0].eval() + list[1].eval()
			case '*': return list[0].eval() * list[1].eval()
			case '-': return list[0].eval() - list[1].eval()
			case '\\/': return list[0].eval() / list[1].eval()
			case '%': return list[0].eval() % list[1].eval()
			case '|': return list[0].eval() || list[1].eval()
			case '&': return list[0].eval() && list[1].eval()
			case 'not': return !list[0].eval()
			case '<': return list[0].eval() < list[1].eval()
			case '>': return list[0].eval() > list[1].eval()
			case '=': return list[0].eval() == list[1].eval()
			case 'randomFrom:to:':
				double l = list[0].eval().toDouble()
				double r = list[1].eval().toDouble()
				if( (Math.round(l) == l) && (Math.round(r) == r) )
				  return l + rnd.nextInt( Math.round(r-l+1).toInteger())
				else
				  return l + (r-l) * rnd.nextDouble()  
			case 'rounded': return list[0].eval().setScale(0,BigDecimal.ROUND_HALF_UP)
		}
	}
}

class ComputeFunction extends Expression {
	Expression op
	public ComputeFunction(String n, Expression exp) {
		super(n)
		op = exp
	}
	public ComputeFunction(String n,BigDecimal o) { this(n, new Number(o)) }
	
	public String toString() { return "[" + '"computeFunction:of:", ' + name + ", "  + op.toString() + "]"  }
	
	def R2D	=		360.0 / (2.0 * Math.PI) as BigDecimal	//57.295779513082320876798154814105 // rad to deg conversion
	def D2R = 		(2.0 * Math.PI) / 360.0 as BigDecimal	//0.01745329251994329576923690768489 // deg to rad conversion
	
	public def eval = {
		switch(nameclean) {
			case 'abs': return op.eval().abs()
			case 'floor': return op.eval().setScale(0,BigDecimal.ROUND_FLOOR)
			case 'ceiling': return op.eval().setScale(0,BigDecimal.ROUND_CEILING)
			case 'sqrt': return Math.sqrt(op.eval())
			case 'sin': return Math.sin(D2R * op.eval())
			case 'cos': return Math.cos(D2R * op.eval())
			case 'tan': return Math.tan(D2R * op.eval())
			case 'asin': return R2D * Math.asin(op.eval())
			case 'acos': return R2D * Math.acos(op.eval())
			case 'atan': return R2D * Math.atan(op.eval())
			case 'ln': return Math.log(op.eval())
			case 'log10': return Math.log10(op.eval())
			case 'e ^': return Math.exp(op.eval())
			case '10 ^': return 10.0 ** op.eval() 
		}
	}
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
	def varIgnore = [] as Set
	public Vars leftShift(String n) { varSet << n ; return this }
	public Vars leftShift(Expression e) { varSet << e.name ; return this }
	public String toString() {
		def b = '"variables": [' << "\n"
		b << (varSet-varIgnore).sort().collect {
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
	def quotation = true
	public VariableS(String n, Expression par) { super(n,par) }
	public VariableS(String n, BigDecimal par) { super(n,par) }
	public VariableS(String n, Expression par, Boolean q) { super(n,par); quotation = q }
	public VariableS(String n, BigDecimal par, Boolean q) { super(n,par); quotation = q }
	public String toString() { 
		if(quotation)
			return '["setVar:to:", ' + name + ', ' + op.toString() + ']'
		else
			return '["setVar:to:", ' + nameclean + ', ' + op.toString() + ']'
	}
	
	def eval = {
		Var.varvals[nameclean] = op.eval()
	}
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
	
	VariableS leftShift(Expression ex) { return new VariableS(nameclean,ex) }
	VariableS leftShift(BigDecimal n) { return new VariableS(nameclean,new Number(n)) }
		
	public String toString() { 
		if(change) 
			return '["changeVar:by:", ' + name + ', ' + changeval.toString() + ']'
		else
			return '["readVariable", ' + name + ']'
	}
	
	def eval = {
		return Var.varvals[nameclean]
	}
}

class StringVar {
	Object getProperty(String property){
		return new Expression(property)
	}
}

class Var {
	public static varvals = [:]
	Object getProperty(String property){
		return new VariableG(property)
	}
	
	Expression set(String name, Expression s) { return new VariableS(name,s,false) }
	Expression set(String name, BigDecimal s) { return new VariableS(name,s,false) }
	Expression set(Expression name, Expression s) { return new VariableS(name.toString(),s,false) }
	Expression set(Expression name, BigDecimal s) { return new VariableS(name.toString(),s,false) }
}

class BlockParameter {
	public static parvals = [:]
	Object getProperty(String property){
		return new Function('getParam',[new Expression(property),new Expression("r")])
	}
}

public abstract class GenerateScratch {

	void init() {}
	
	public GenerateScratch() {
		BigDecimal.metaClass.plus = {Expression x -> if(x.getClass() == Number) new Number(delegate + x.val) else new Number(delegate) + x}
		BigDecimal.metaClass.minus = {Expression x -> if(x.getClass() == Number) new Number(delegate - x.val) else new Number(delegate) - x}
		BigDecimal.metaClass.multiply = {Expression x -> if(x.getClass() == Number) new Number(delegate * x.val) else new Number(delegate) * x}
		BigDecimal.metaClass.div = {Expression x -> if(x.getClass() == Number) new Number(delegate / x.val) else new Number(delegate) / x}
		BigDecimal.metaClass.mod = {Expression x -> if(x.getClass() == Number) new Number(delegate % x.val) else new Number(delegate) % x}
		BigDecimal.metaClass.or = {Expression x -> if(x.getClass() == Number) new Number(delegate | x.val) else new Number(delegate) | x}
		BigDecimal.metaClass.and = {Expression x -> if(x.getClass() == Number) new Number(delegate & x.val) else new Number(delegate) & x}
		init();
	}
	
	def procaliases = [:]

	// Script handling
	def position = 0
	public def Script = { blocks ->
		position += 100
		"[" + position + "," + position + ", [\n\n" + blocks.collect { it.toString() }.join(",\n") + "\n\n]]\n"
	} 

	public def printScripts =  { scripts ->
		println '"scripts": [' + "\n" + scripts.join(",\n") + "\n]\n"
	}

	public def printVariables = { println VariableG.varSet.toString() }

// Custom blocks with hack, look here: http://scratch.mit.edu/discuss/topic/1810/
// Parameter types list: https://docs.google.com/spreadsheet/ccc?key=0Ai13BQTlMxCzdG5lelZDczFnc241S2FmWVNhcEkwMEE#gid=0
// You should provide block name with as a string with parameter types fe. '"calculate sum %n and %n and store it to %m.var"',["par1","par2","variable"]

	public def Def = { alias, name, List pars, List blocks -> 
		procaliases[alias] = name
		  
		Function.defbodies[name] = [pars,blocks]
		
		Script( [new Function('procDef', [new Expression(name), 
			new Expression(pars.collect{ '"' + it + '"'}.toString()).noquotation(),
			new Expression(pars.collect{ '"1"' }.toString()).noquotation(),
			new Expression('true')]
		)] + blocks )
	}

	public def Call = { alias, List pars -> 
		new Function('call', [new Expression(procaliases[alias])] + pars)
	}

	// Aliases
	public def V = new Var() // Variable
	public def Par = new BlockParameter() // Block Parameter
	public def N = { x -> new Number(x) } // number
	public def S = new StringVar() // string, used in function call for naming variables in %m.val type
	public def LT = { l, r -> new Function('<',[l,r]) }
	public def GT = { l, r -> new Function('>',[l,r]) }
	public def EQ = { l, r -> new Function('=',[l,r]) }
	
	public def Abs = { par -> new ComputeFunction('abs',par) }
	public def Floor = { par -> new ComputeFunction('floor',par) }
	public def Ceiling = { par -> new ComputeFunction('ceiling',par) }
	public def Sqrt = { par -> new ComputeFunction('sqrt',par) }
	public def Sin = { par -> new ComputeFunction('sin',par) }
	public def Cos = { par -> new ComputeFunction('cos',par) }
	public def Tan = { par -> new ComputeFunction('tan',par) }
	public def ASin = { par -> new ComputeFunction('asin',par) }
	public def ACos = { par -> new ComputeFunction('acos',par) }
	public def ATan = { par -> new ComputeFunction('atan',par) }
	public def Ln = { par -> new ComputeFunction('ln',par) }
	public def Log = { par -> new ComputeFunction('log',par) }
	public def Exp = { par -> new ComputeFunction('e ^',par) }
	public def Pow10 = { par -> new ComputeFunction('10 ^',par) }
	public def Round = { par -> new Function('rounded', [par]) }
	public def Rnd = { from, to -> new Function('randomFrom:to:',[from,to]) }
	public def Rnd01 = Rnd(0.001,1.0)
	
	public def IfElse = { cond,op1,op2 -> new Command('doIfElse',cond,op1,op2) }
	public def If = { cond,op1 -> new Command('doIf',cond,op1) }
	
	// Math aliases
	public def M_E = 		Math.E as BigDecimal					//2,7182818284590452354
	public def M_LOG2E = 	1.0 / Math.log(2.0) as BigDecimal 	//1.4426950408889634074
	public def M_LOG10E = 	Math.log10(Math.E) as BigDecimal		//0.43429448190325182765
	public def M_LN2 =		Math.log(2.0) as BigDecimal			//0.69314718055994530942
	public def M_LN10 =	Math.log(10.0) as BigDecimal 			//2.30258509299404568402
	public def M_PI =		Math.PI as BigDecimal 				//3.14159265358979323846
	public def M_PI_2 =	Math.PI / 2.0 as BigDecimal 			//1.57079632679489661923
	public def M_PI_4 =	Math.PI / 4.0 as BigDecimal 			//0.78539816339744830962
	public def M_1_PI =	1.0 / Math.PI as BigDecimal 			//0.31830988618379067154
	public def M_2_PI =	2.0 / Math.PI as BigDecimal 			//0.63661977236758134308
	public def M_2_SQRTPI =2.0 / Math.sqrt(Math.PI) as BigDecimal //1.12837916709551257390
	public def M_SQRT2 =	Math.sqrt(2.0) as BigDecimal 			//1.41421356237309504880
	public def M_SQRT1_2 =	Math.sqrt(2.0) / 2.0 as BigDecimal 	//0.7071067811865475244
	public def M_2PI =     2.0 * Math.PI as BigDecimal 			//6.283185307179586476925286766559
	public def M_3PI_4 =   3.0 * Math.PI / 4.0 as BigDecimal 	//2.3561944901923449288469825374596
	public def M_SQRT3 =   Math.sqrt(3.0) as BigDecimal 			//1.7320508075688772935274463415059
	public def M_SQRT3_2 = Math.sqrt(3.0) / 2.0 as BigDecimal 	//0.86602540378443864676372317075249
	public def M_SQRT5 =   Math.sqrt(5.0) as BigDecimal			//2.2360679774997896964091736687313
	public def M_PHI =     (1.0 + Math.sqrt(5.0)) / 2.0 as BigDecimal 	//1.61803398874989484820458683436563 // golden ratio
	public def M_1_2PI =   1.0 / (2.0 * Math.PI) as BigDecimal	//0.1591549430918953357688837633725
	public def IM =		2147483647.0 as BigDecimal
	public def AM = 		1.0 / IM as BigDecimal 				//0.000000000465661287524579692410575
	public def R2D	=		360.0 / (2.0 * Math.PI) as BigDecimal	//57.295779513082320876798154814105 // rad to deg conversion
	public def D2R = 		(2.0 * Math.PI) / 360.0 as BigDecimal	//0.01745329251994329576923690768489 // deg to rad conversion
}


