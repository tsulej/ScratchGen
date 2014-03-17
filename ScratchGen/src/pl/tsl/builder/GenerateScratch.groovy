// This script is used to generate Scratch json scripts to easier managing bigger projects

package pl.tsl.builder

BigDecimal.metaClass.plus = {Expression x -> if(x.getClass() == Number) new Number(delegate + x.val) else new Number(delegate) + x}
BigDecimal.metaClass.minus = {Expression x -> if(x.getClass() == Number) new Number(delegate - x.val) else new Number(delegate) - x}
BigDecimal.metaClass.multiply = {Expression x -> if(x.getClass() == Number) new Number(delegate * x.val) else new Number(delegate) * x}
BigDecimal.metaClass.div = {Expression x -> if(x.getClass() == Number) new Number(delegate / x.val) else new Number(delegate) / x}
BigDecimal.metaClass.mod = {Expression x -> if(x.getClass() == Number) new Number(delegate % x.val) else new Number(delegate) % x}
BigDecimal.metaClass.or = {Expression x -> if(x.getClass() == Number) new Number(delegate | x.val) else new Number(delegate) | x}
BigDecimal.metaClass.and = {Expression x -> if(x.getClass() == Number) new Number(delegate & x.val) else new Number(delegate) & x}

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
	
	Expression negative() { new Number(-1) * this }
	Expression bitwiseNegate() { new ComputeFunction('not',this) }
	
	public Expression(String n) { name = '"' + n + '"'; nameclean = n }
	public String toString() { return name } 
	public Expression noquotation() { name = nameclean; return this }
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
	def varIgnore = [] as Set
	public Vars leftShift(String n) { varSet << n ; return this }
	public Vars leftShift(Expression e) { varSet << e.name ; return this }
	public String toString() {
		def b = '"variables": [' << "\n"
		b << (varSet-varIgnore).collect {
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
}

class StringVar {
	Object getProperty(String property){
		return new Expression(property)
	}
}

class Var {
	Object getProperty(String property){
		return new VariableG(property)
	}
	
	Expression set(String name, Expression s) { return new VariableS(name,s,false) }
	Expression set(String name, BigDecimal s) { return new VariableS(name,s,false) }
	Expression set(Expression name, Expression s) { return new VariableS(name.toString(),s,false) }
	Expression set(Expression name, BigDecimal s) { return new VariableS(name.toString(),s,false) }
}

class BlockParameter {
	Object getProperty(String property){
		return new Function('getParam',[new Expression(property),new Expression("r")])
	}
}

def procaliases = [:]

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

// Custom blocks with hack, look here: http://scratch.mit.edu/discuss/topic/1810/
// Parameter types list: https://docs.google.com/spreadsheet/ccc?key=0Ai13BQTlMxCzdG5lelZDczFnc241S2FmWVNhcEkwMEE#gid=0
// You should provide block name with as a string with parameter types fe. '"calculate sum %n and %n and store it to %m.var"',["par1","par2","variable"]

def Def = { alias, name, List pars, List blocks -> procaliases[alias] = name ; 
	Script( [new Function('procDef', [new Expression(name), 
		new Expression(pars.collect{ '"' + it + '"'}.toString()).noquotation(),
		new Expression(pars.collect{ '""' }.toString()).noquotation(),
		new Expression('true')]
	)] + blocks )
}

def Call = { alias, List pars -> 
	new Function('call', [new Expression(procaliases[alias])] + pars)
}

// Aliases
def V = new Var() // Variable
def Par = new BlockParameter() // Block Parameter
def N = { x -> new Number(x) } // number
def S = new StringVar() // string, used in function call for naming variables in %m.val type
def LT = { l, r -> new Function('<',[l,r]) }
def GT = { l, r -> new Function('>',[l,r]) }
def EQ = { l, r -> new Function('=',[l,r]) }

def Abs = { par -> new ComputeFunction('abs',par) }
def Floor = { par -> new ComputeFunction('floor',par) }
def Ceiling = { par -> new ComputeFunction('ceiling',par) }
def Sqrt = { par -> new ComputeFunction('sqrt',par) }
def Sin = { par -> new ComputeFunction('sin',par) }
def Cos = { par -> new ComputeFunction('cos',par) }
def Tan = { par -> new ComputeFunction('tan',par) }
def ASin = { par -> new ComputeFunction('asin',par) }
def ACos = { par -> new ComputeFunction('acos',par) }
def ATan = { par -> new ComputeFunction('atan',par) }
def Ln = { par -> new ComputeFunction('ln',par) }
def Log = { par -> new ComputeFunction('log',par) }
def Exp = { par -> new ComputeFunction('e ^',par) }
def Pow10 = { par -> new ComputeFunction('10 ^',par) }
def Round = { par -> new Function('rounded', [par]) }
def Rnd = { from, to -> new Function('randomFrom:to:',[from,to]) }
def Rnd01 = Rnd(0.001,1.0)

def IfElse = { cond,op1,op2 -> new Command('doIfElse',cond,op1,op2) }
def If = { cond,op1 -> new Command('doIf',cond,op1) }

// Math aliases
def M_E = 		2.7182818284590452354
def M_LOG2E = 	1.4426950408889634074
def M_LOG10E = 	0.43429448190325182765
def M_LN2 =		0.69314718055994530942
def M_LN10 =	2.30258509299404568402
def M_PI =		3.14159265358979323846
def M_PI_2 =	1.57079632679489661923
def M_PI_4 =	0.78539816339744830962
def M_1_PI =	0.31830988618379067154
def M_2_PI =	0.63661977236758134308
def M_2_SQRTPI =1.12837916709551257390
def M_SQRT2 =	1.41421356237309504880
def M_SQRT1_2 =	0.7071067811865475244
def M_2PI =     6.283185307179586476925286766559
def M_3PI_4 =   2.3561944901923449288469825374596
def M_SQRT3 =   1.7320508075688772935274463415059
def M_SQRT3_2 = 0.86602540378443864676372317075249
def M_SQRT5 =   2.2360679774997896964091736687313
def M_PHI =     1.61803398874989484820458683436563 // golden ratio
def M_1_2PI =   0.1591549430918953357688837633725
def IM =		2147483647.0
def AM = 		0.000000000465661287524579692410575
def R2D	=		57.295779513082320876798154814105 // rad to deg conversion
def D2R = 		0.01745329251994329576923690768489 // deg to rad conversion

// Your script generation goes here

// do not display below variables, they are already defined
VariableG.varSet.varIgnore += ['x','y','r','r2','par1','par2','par3','par4','par5','atan2xy','atan2yx','_a','_b','_c','_d','_e','_f','_weight'] as Set

Scripts([
	
	Def("pow", 'power %n ^ %n into %m.var',['X','A','VAR'],
		[ V.set( Par.VAR, Exp( Par.A * Ln( Par.X)))
		]
	),

	Def("sinh", 'sinh %n into %m.var',['X','VAR'], // Result in Radians
		[ V.set( Par.VAR, 0.5 * ( Exp(Par.X) - Exp( 0.0 - Par.X)))
		]
	),
	
	Def("cosh", 'cosh %n into %m.var',['X','VAR'], // Result in Radians
		[ V.set( Par.VAR, 0.5 * ( Exp(Par.X) + Exp( 0.0 - Par.X)))
		]
	),
	
	Def("atan2", 'atan2 %n %n into %m.var',['Y','X','VAR'], // Result in Degrees
		[ IfElse( GT( Par.X, 0.0),
			[ V.set( Par.VAR, ATan(Par.Y / Par.X))],
			[ IfElse( LT( Par.X, 0.0),
				[ IfElse( GT( Par.Y, 0.0) | EQ( Par.Y, 0.0),
					[ V.set( Par.VAR, 180.0 + ATan(Par.Y / Par.X)) ],
					[ V.set( Par.VAR, -180.0 + ATan(Par.Y / Par.X))]
				  )
				],
				[ IfElse( GT( Par.Y, 0.0),
					[ V.set( Par.VAR, 90.0)],
					[ IfElse( LT( Par.Y, 0.0),
						[ V.set( Par.VAR, -90.0)],
						[ V.set( Par.VAR, 0.0)]
					  ) 
					]
				 )
				]
				) 
			]
		  )
		]
	),

	Def("calc_0_9", 'calculate 0-9 %n %n %n',['func','X','Y'],
	[
		IfElse( EQ( Par.func,0), // Linear
			[ V.x << Par.X,
			  V.y << Par.Y
			],
			[IfElse ( EQ( Par.func, 1), // Sinusoidal
				[ V.x << Sin( R2D * Par.X),
				  V.y << Sin( R2D * Par.Y)
				],
				[IfElse ( EQ( Par.func, 2), // Spherical
					[ V.x << Par.X / V.r2,
					  V.y << Par.Y / V.r2
					],
					[IfElse ( EQ( Par.func, 3), // Swirl
						[ V.par3 << R2D * V.r2,
						  V.par1 << Sin( V.par3 ),
						  V.par2 << Cos( V.par3 ),
						  V.x << V.par1 * Par.X - V.par2 * Par.Y,
						  V.y << V.par2 * Par.X + V.par1 * Par.Y
						],
						[IfElse ( EQ( Par.func, 4), // Horseshoe
							[ V.x << ( (Par.X - Par.Y) * ( Par.X + Par.Y) ) / V.r,
							  V.y << (2.0 * Par.X * Par.Y) / V.r	
							],
							[IfElse ( EQ( Par.func, 5), // Polar
								[ V.x <<  M_1_PI * D2R * V.atan2xy,
								  V.y << V.r - 1.0
								],
								[IfElse ( EQ(Par.func,6), // Handkerchief
									[ V.par1 << V.r * R2D,
									  V.x << V.r * Sin( V.atan2xy + V.par1),
									  V.y << V.r * Cos( V.atan2xy - V.par1),
									],
									[IfElse ( EQ( Par.func,7), // Heart
										[ V.par1 << V.r * V.atan2xy,
										  V.x << V.r * Sin(V.par1),
										  V.y << -(V.r) * Cos(V.par1)
										],
										[IfElse ( EQ( Par.func,8), // Disc
											[ V.par1 << D2R * M_1_PI * V.atan2xy,
											  V.par2 << M_PI * R2D * V.r,
											  V.x << Sin(V.par2) * V.par1,
											  V.y << Cos(V.par2) * V.par1,
											],
											[ V.par1 << R2D * V.r,
											  V.x << (Cos(V.atan2xy) + Sin(V.par1)) / V.r, // Spiral
											  V.y << (Sin(V.atan2xy) - Cos(V.par2)) / V.r
											]
										)]
									)]
								)]
							)]
						)]
					)]
				)]	  	
			)]
		)
	]
	),

	Def("calc_10_19", 'calculate 10-19 %n %n %n',['func','X','Y'],
	[
		IfElse( EQ( Par.func,10), // Hyperbolic
			[ V.x << Sin(V.atan2xy) / V.r,
			  V.y << Cos(V.atan2xy) * V.r
			],
			[IfElse ( EQ( Par.func, 11), // Diamond
				[ V.par1 << R2D * V.r,
				  V.par2 << Sin(V.par1),
				  V.par3 << Cos(V.par1),
				  V.x << Sin(V.atan2xy) * V.par3,
				  V.y << Cos(V.atan2xy) * V.par2	
				],
				[IfElse ( EQ( Par.func, 12), // Ex
					[ V.par1 << R2D * V.r,
					  V.par2 << Sin(V.atan2xy + V.par1),
					  V.par3 << Cos(V.atan2xy - V.par1),
					  V.par4 << V.par2 * V.par2 * V.par2 * V.r,
					  V.par5 << V.par3 * V.par3 * V.par3 * V.r,
					  V.x << V.par4 + V.par5,
					  V.y << V.par4 - V.par5
					],
					[IfElse ( EQ( Par.func, 13), // Julia
						[ V.par1 << Sqrt(V.r),
						  V.par2 << 0.5 * V.atan2xy + (Rnd(0,1) * 180.0),
						  V.x << V.par1 * Cos(V.par2),
						  V.y << V.par1 * Sin(V.par2)	
						],
						[IfElse ( EQ( Par.func, 14), // Bent 
							[ IfElse( LT(Par.X,0), [V.x << Par.X * 2.0], [ V.x << Par.X]),
							  IfElse( LT(Par.Y,0), [V.y << Par.Y / 2.0], [ V.y << Par.Y])
							],
							[IfElse ( EQ( Par.func, 15), // Waves
								[ 	V.x << Par.X + V._b * Sin( R2D * Par.Y * (1.0 / (V._c * V._c)) ),
									V.y << Par.Y + V._e * Sin( R2D * Par.X * (1.0 / (V._f * V._f)) ),
								],
								[IfElse ( EQ(Par.func, 16), // Fisheye 
									[ V.par1 << 2.0 / (V.r + 1.0),
									  V.x << V.par1 * Par.Y,
									  V.y << V.par1 * Par.X
									],
									[IfElse ( EQ( Par.func, 17), // Popcorn
										[ V.par1 << R2D * 3.0,
										  V.x << Par.X + V._c * Sin(R2D * Tan(V.par1 * Par.Y)),
										  V.y << Par.Y + V._f * Sin(R2D * Tan(V.par1 * Par.X)),
										],
										[IfElse ( EQ( Par.func, 18), // Exponential 
											[ V.par1 << Exp(Par.X - 1.0),
											  V.par2 << R2D * M_PI * Par.Y,
											  V.x << V.par1 * Cos(V.par2),
											  V.y << V.par1 * Sin(V.par2)	
											],
											[ V.par1 << Sin(V.atan2xy), // Power 
											  Call("pow",[V.r, V.par1, S.par2]),
											  V.x << V.par2 * Cos(V.atan2xy),
											  V.y << V.par2 * V.par1
											]
										)]
									)]
								)]
							)]
						)]
					)]
				)]
			)]
		)
	]
	),

	Def("calc_20_29", 'calculate 20-29 %n %n %n',['func','X','Y'],
	[
		IfElse( EQ( Par.func,20), // Cosine
			[ V.par1 << Par.X * M_PI * R2D,
			  Call("cosh",[Par.Y, S.par2]),
			  Call("sinh",[Par.Y, S.par3]),
			  V.x << Cos(V.par1) * V.par2,
			  V.y << -(Sin(V.par1)) * V.par3 
			],
			[IfElse ( EQ( Par.func, 21), // Rings
				[ V.par1 << V._c * V._c,
				  V.par2 << ( (V.r + V.par1) % (2.0 * V.par1)) - V.par1 + V.r * (1.0 - V.par1),
				  V.x << V.par2 * Cos(V.atan2xy),
				  V.y << V.par2 * Sin(V.atan2xy)
				],
				[IfElse ( EQ( Par.func, 22), // Fan
					[ V.par1 << 0.5 * M_PI * R2D * V._c * V._c,
					  IfElse( GT( (V.atan2xy + R2D * V._f) % (2.0*V.par1), V.par1 ),
					    [V.par2 << V.atan2xy - V.par1],
						[V.par2 << V.atan2xy + V.par1]
					  ),
				     V.x << V.r * Cos(V.par2),
					 V.y << V.r * Sin(V.par2)
					],
					[IfElse ( EQ( Par.func, 23), // Blob
						[ V.par1 << V.r * (V._blob_low + V._blob_bdiff * (0.5 + 0.5 * Sin(V._blob_waves * V.atan2xy))),
						  V.x << Sin(V.atan2xy) * V.par1,
						  V.y << Cos(V.atan2xy) * V.par1
						],
						[IfElse ( EQ( Par.func, 24), // PDJ
							[ V.x << Sin(V._pdj_a * Par.Y) - Cos(V._pdj_b * Par.X),
							  V.y << Sin(V._pdj_c * Par.X) - Cos(V._pdj_d * Par.Y),
							],
							[IfElse ( EQ( Par.func, 25), // Fan2
								[ V.par1 << V.atan2xy + V._fan2_y - V._fan2_x * Floor( (V.atan2xy + V._fan2_y)/V._fan2_x),
								  V.par2 << 0.5 * V._fan2_x,
								  IfElse( GT(V.par1, V.par2),
									  [V.par3 << V.atan2xy - V.par2],
									  [V.par3 << V.atan2xy + V.par2]
								  ),
								  V.x << V.r * Sin(V.par3),
								  V.y << V.r * Cos(V.par3)
								],
								[IfElse ( EQ(Par.func, 26), // Rings2
									[ V.par1 << V.r - 2.0 * V._rings2_val * Floor( (V.r + V._rings2_val) / (2.0 * V._rings2_val)) + V.r * (1.0 - V._rings2_val),
									  V.x << Sin(V.atan2xy) * V.par1,
									  V.y << Cos(V.atan2xy) * V.par1
									],
									[IfElse ( EQ( Par.func, 27), // Eyefish
										[ V.par1 << 2.0 / (V.r + 1.0),
										  V.x << Par.X * V.par1,
										  V.y << Par.Y * V.par1
										],
										[IfElse ( EQ( Par.func, 28), // Bubble
											[ V.par1 << 4.0 / (V.r2 + 4.0),
											  V.x << V.par1 * Par.X,
											  V.y << V.par1 * Par.Y
											],
											[ V.x << Sin(R2D * Par.X), // Cylinder
											  V.y << Par.Y
											]
										)]
									)]
								)]
							)]
						)]
					)]
				)]
			)]
		)
	]
	),

	Def("calc_30_39", 'calculate 30-39 %n %n %n',['func','X','Y'],
	[
		IfElse( EQ( Par.func,30), // Perspective
			[ V.par1 << 1.0 / ( V._perspective_dist - Par.Y * V._perspective_vsin),
			  V.x << V._perspective_dist * Par.X * V.par1,
			  V.y << V._perspective_vfcos * Par.Y * V.par1
			],
			[IfElse ( EQ( Par.func, 31), // Noise
				[ V.par1 << 2 * M_PI * R2D * Rnd01,
				  V.par2 << Rnd01,
				  V.x << Par.X * V.par2 * Cos(V.par1),
				  V.y << Par.Y * V.par2 * Sin(V.par1)
				],
				[IfElse ( EQ( Par.func, 32), // JuliaN
					[ V.par1 << ( V.atan2yx + 2.0 * M_PI * R2D * Floor( V._julian_power * Rnd01)) / V._julian_power,
					  Call("pow", [V.r2,V._julian_cn,S.par2]),
					  V.x << V.par2 * Cos(V.par1),
					  V.y << V.par2 * Sin(V.par1)
					],
					[IfElse ( EQ( Par.func, 33), // JuliaScope
						[ V.par2 << Floor( V._juliascope_power * Rnd01),
						  IfElse( EQ( V.par2 % 2, 0.0),
							  [V.par1 << ( 2.0 * M_PI * R2D * V.par2 + V.atan2yx) / V._juliascope_power],
							  [V.par1 << ( 2.0 * M_PI * R2D * V.par2 - V.atan2yx) / V._juliascope_power]
						  ),
					      Call("pow", [V.r2,V._juliascope_cn,S.par2]),
					      V.x << V.par2 * Cos(V.par1),
					      V.y << V.par2 * Sin(V.par1)
						],
						[IfElse ( EQ( Par.func, 34), // Blur
							[ V.par1 << 2 * M_PI * R2D * Rnd01,
							  V.par2 << Rnd01,
							  V.x << V.par2 * Cos(V.par1),
							  V.y << V.par2 * Sin(V.par1)
							],
							[IfElse ( EQ( Par.func, 35), // Gaussian
								[ V.par1 << 2 * M_PI * R2D * Rnd01,
								  V.par2 << Rnd01 + Rnd01 + Rnd01 + Rnd01 - 2.0,
								  V.x << V.par2 * Cos(V.par1),
								  V.y << V.par2 * Sin(V.par1)
								],
								[IfElse ( EQ(Par.func, 36), // RadialBlur
									[ V.par1 << V._weight * (Rnd01 + Rnd01 + Rnd01 + Rnd01 - 2.0),
									  V.par2 << V.atan2yx + R2D * V._radialblur_spinvar * V.par1,
									  V.par3 << V._radialblur_zoomvar * V.par1 - 1.0,
									  V.x << ( V.r * Cos(V.par2) + V.par3 * Par.X) / V._weight,
									  V.y << ( V.r * Sin(V.par2) + V.par3 * Par.Y) / V._weight,
									],
									[IfElse ( EQ( Par.func, 37), // Pie
										[ V.par1 << R2D * (V._pie_rotation + 2.0 * M_PI * (Floor(Rnd01 * V._pie_slices + 0.5) + Rnd01 * V._pie_thickness) / V._pie_slices),
										  V.par2 << Rnd01,
										  V.x << V.par2 * Cos(V.par1),
										  V.y << V.par2 * Sin(V.par1)
										],
										[IfElse ( EQ( Par.func, 38), // Ngon
											[ V.par1 << V.atan2yx - ( V._ngon_sides * Floor(V.atan2yx / V._ngon_sides)),
											  If( GT (V.par1, V._ngon_sides / 2.0),
												  [V.par1 << V.par1 - V._ngon_sides]
										      ),
										  	  Call("pow", [V.r2, V._ngon_power, S.par3]),
											  V.par2 << (V._ngon_corners * (1.0 / Cos(V.par1) - 1.0) + V._ngon_circle) / V.par3,
											  V.x << Par.X * V.par2,
											  V.y << Par.Y * V.par2
											],
											[ V.par1 << 1.0 + V._curl_c1 * Par.X + V._curl_c2 * (Par.X * Par.X - Par.Y * Par.Y), // Curl
											  V.par2 << V._curl_c1 * Par.Y + 2.0 * V._curl_c2 * Par.X * Par.Y,
											  V.par3 << 1.0 / (V.par1 * V.par1 + V.par2 * V.par2),
											  V.x << (Par.X * V.par1 + Par.Y * V.par2) * V.par3,
											  V.y << (Par.Y * V.par1 - Par.X * V.par2) * V.par3
											]
										)]
									)]
								)]
							)]
						)]
					)]
				)]
			)]
		)
	]
	),

	Def("calc_40_49", 'calculate 40-49 %n %n %n',['func','X','Y'],
	[
		IfElse( EQ( Par.func, 40), // Rectangles
			[ IfElse( EQ (V._rectangles_x, 0),
				[ V.x << Par.X],
				[ V.x << (2.0 * Floor(Par.X / V._rectangles_x) + 1.0) * V._rectangles_x - Par.X ]
			  ),
		  	  IfElse( EQ (V._rectangles_y, 0),
				[ V.y << Par.Y],
				[ V.y << (2.0 * Floor(Par.Y / V._rectangles_y) + 1.0) * V._rectangles_y - Par.Y ]
			  )
			],
			[IfElse ( EQ( Par.func, 41), // Arch
				[ V.par1 <<  M_PI * R2D * Rnd01 * V._weight,
				  V.par2 << Sin(V.par1),
				  V.par3 << Cos(V.par1),
				  V.x << V.par2,
				  V.y << (V.par2 * V.par2) / V.par3
				],
				[IfElse ( EQ( Par.func, 42), // Tangent
					[ V.x << Sin(Par.X * R2D) / Cos(Par.Y * R2D),
					  V.y << Tan(Par.Y * R2D)
					],
					[IfElse ( EQ( Par.func, 43), // Square
						[ V.x << Rnd01 - 0.5,
						  V.y << Rnd01 - 0.5
						],
						[IfElse ( EQ( Par.func, 44), // Rays
							[ V.par1 << M_PI * R2D * V._weight * Rnd01,
							  V.par2 << Tan(V.par1) * V._weight / V.r2,
							  V.x << V.par2 * Cos(Par.X * R2D),
							  V.y << V.par2 * Sin(Par.Y * R2D)
							],
							[IfElse ( EQ( Par.func, 45), // Blade
								[ V.par1 << R2D * Rnd01 * V._weight * V.r,
								  V.par2 << Sin(V.par1),
								  V.par3 << Cos(V.par1),
								  V.x << Par.X * (V.par3 + V.par2),
								  V.y << Par.X * (V.par3 - V.par2),
								],
								[IfElse ( EQ(Par.func, 46), // Secant
									[ V.par1 << Cos ( R2D * V._weight * V.r),
									  V.par2 << 1.0 / V.par1,
								      V.x << Par.X,
									  IfElse( LT(V.par1, 0),
										  [ V.y << V.par2 + 1.0],
										  [ V.y << V.par2 - 1.0]
									  )
									],
									[IfElse ( EQ( Par.func, 47), // Twintrian
										[ V.par1 << R2D * Rnd01 * V._weight * V.r,
										  V.par2 << Sin(V.par1),
										  V.par3 << Log(V.par2 * V.par2) + Cos(V.par1),
										  If( LT(V.par3,-30), [V.par3 << -30.0] ),
										  V.x << Par.X * V.par3,
										  V.y << Par.X * (V.par3 - V.par2 * M_PI) 
										],
										[IfElse ( EQ( Par.func, 48), // Cross
											[ V.par1 << Par.X * Par.X - Par.Y * Par.Y,
											  V.par2 << Sqrt(1.0 / (V.par1 * V.par1)),
											  V.x << Par.X * V.par2,
											  V.y << Par.Y * V.par2
											],
											[ V.par1 << V._disc2_timespi * (Par.X + Par.Y), // Disc2
											  V.par2 << 1.0 / M_PI * D2R * V.atan2xy,
											  V.x << (Sin(V.par1) + V._disc2_cosadd) * V.par2,
											  V.y << (Cos(V.par1) + V._disc2_sinadd) * V.par2 
											]
										)]
									)]
								)]
							)]
						)]
					)]
				)]
			)]
		)
	]
	),

  Def("calc_0_9", 'calculate 0-9 %n %n %n',['func','X','Y'],
	[
		IfElse( EQ( Par.func,0), //
			[ //here
			],
			[IfElse ( EQ( Par.func, 1), //
				[ //here
				],
				[IfElse ( EQ( Par.func, 2), //
					[ //here
					],
					[IfElse ( EQ( Par.func, 3), //
						[ //here
						],
						[IfElse ( EQ( Par.func, 4), //
							[ //here
							],
							[IfElse ( EQ( Par.func, 5), //
								[ //here
								],
								[IfElse ( EQ(Par.func, 6), //
									[ //here
									],
									[IfElse ( EQ( Par.func, 7), //
										[ //here
										],
										[IfElse ( EQ( Par.func, 8), //
											[ //here
											],
											[ // here
											]
										)]
									)]
								)]
							)]
						)]
					)]
				)]
			)]
		)
	]
	),

    Script([  // precalculations
		// Blob
		V._blob_low << Rnd(0.2, 0.7),
		V._blob_bdiff << Rnd(0.8, 1.2) - V._blob_low,
		V._blob_waves << Rnd(2,7),
		// PDJ
		V._pdj_a << R2D * Rnd(-2.99, 2.99),
		V._pdj_b << R2D * Rnd(-2.99, 2.99),
		V._pdj_c << R2D * Rnd(-2.99, 2.99),
		V._pdj_d << R2D * Rnd(-2.99, 2.99),
		// Fan2
		V._fan2_x << R2D * M_PI * Rnd(0.04,0.64),
		V._fan2_y << R2D * Rnd(2,7),
		// Rings2
		V._rings2_val << Rnd(0.01,1.44),
		// Perspective
		V._perspective_dist << Rnd(0.99,3.01),
		V.par1 << R2D * M_PI / 2.0 * Rnd(0.3,1.0),
		V._perspective_vsin << Sin(V.par1),
		V._perspective_vfcos << V._perspective_dist * Cos(V.par1),
		// JuliaN
		V._julian_power << Rnd(4,10),
		V._julian_dist << Rnd(0.5,2.0),
		V._julian_cn << V._julian_dist / V._julian_power / 2.0,
		// JuliaScope
		V._juliascope_power << Rnd(4,10),
		V._juliascope_dist << Rnd(0.5,2.0),
		V._juliascope_cn << V._juliascope_dist / V._juliascope_power / 2.0,
		// RadialBlur
		V.par1 << R2D * M_PI / 2.0 * Rnd(-1.01,1.01),
		V._radialblur_spinvar << Sin(V.par1),
		V._radialblur_zoomvar << Cos(V.par1),
		// Pie
		V._pie_slices << Rnd(3,10),
		V._pie_rotation << Rnd(-2.01,2.01),
		V._pie_thickness << Rnd(0.2,0.8),
		// Ngon
		V._ngon_sides << 2.0 * M_PI * R2D / Rnd(3,9),
		V._ngon_power << Rnd(1.01, 4.0) / 2.0,
		V._ngon_circle << Rnd(0.5, 1.5),
		V._ngon_corners << Rnd(0.5, 1.5),
		// Curl
		V._curl_c1 << Rnd(0.1, 0.7),
		V._curl_c2 << Rnd(0.1, 0.7),
		// Rectangles
		V._rectangles_x << Rnd(0.01,1),
		V._rectangles_y << Rnd(0.01,1),
		// Disc2
		V._disc2_timespi << Rnd(-2.01,2.01) * M_PI * R2D,
		V.par1 << Rnd(-8.01,8.01),
		V._disc2_sinadd << Sin(V.par1 * R2D),
		V._disc2_cosadd << Cos(V.par1 * R2D) - 1.0,
		If( GT ( V.par1, 2.0 * M_PI),
			[ V.par2 << 1.0 - 2 * M_PI + V.par1,
				V._disc2_sinadd << V._disc2_sinadd * V.par2,
				V._disc2_cosadd << V._disc2_cosadd * V.par2
			]
		),
		If( LT ( V.par1, -2.0 * M_PI),
			[ V.par2 << 1.0 + 2 * M_PI + V.par1,
				V._disc2_sinadd << V._disc2_sinadd * V.par2,
				V._disc2_cosadd << V._disc2_cosadd * V.par2
			]
		)
	])
	
	
		 /*
		  * 
	
						["append:toList:", ["\/", ["randomFrom:to:", 0, 1000], 1000], "parameters"],
						["append:toList:", ["*", ["randomFrom:to:", 1, 6], 0.25], "parameters"],
						["append:toList:", ["\/", -1, ["\/", ["randomFrom:to:", 1, 10000], 1000]], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", 0, 20000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", 0, 20000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -1000, 1000], 1000], "parameters"],
						["append:toList:", ["randomFrom:to:", 3, 8], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -500, 200], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", 300, 1000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", 0, 1000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", 500, 2000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", 500, 2000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -1500, 1500], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -1500, 1500], 1000], "parameters"],
						["append:toList:",
							["*",
								["*", -1, ["readVariable", "pi2"]],
								["\/", ["randomFrom:to:", -1500, 1500], 1000]],
							"parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -2500, 2500], 1000], "parameters"],
						["append:toList:", ["randomFrom:to:", 1, 5], "parameters"],
						["append:toList:",
							["\/", ["\/", ["randomFrom:to:", 1000, 3000], 1000], ["getLine:ofList:", 47, "parameters"]],
							"parameters"],
						["append:toList:",
							["\/", ["\/", ["randomFrom:to:", -500, 500], 1000], ["getLine:ofList:", 47, "parameters"]],
							"parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -2500, 2500], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -2500, 2500], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", 800, 9000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", 800, 9000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -3000, 3000], 1000], "parameters"],
						["append:toList:",
							["computeFunction:of:", "cos", ["*", ["readVariable", "radtodeg"], ["getLine:ofList:", 54, "parameters"]]],
							"parameters"],
						["setLine:ofList:to:",
							54,
							"parameters",
							["computeFunction:of:", "sin", ["*", ["readVariable", "radtodeg"], ["getLine:ofList:", 54, "parameters"]]]],
						["append:toList:", ["\/", ["randomFrom:to:", -3000, 3000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -2000, 1000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -2000, 2000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -500, 500], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -500, 500], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -1000, 1000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -1000, 1000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", 0, 2000], 1000], "parameters"],
						["append:toList:", ["*", 360, ["\/", ["randomFrom:to:", -3000, 3000], 1000]], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", 1000, 3000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", 0, 1000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -400, 400], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -400, 400], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -5000, 5000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", 0, 1000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -1000, 1000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", 0, 1000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -1000, 1000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -1000, 1000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -1000, 1000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -1000, 1000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -1000, 1000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -800, 800], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -4000, 4000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -3000, 3000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -500, 500], 1000], "parameters"],
						["append:toList:", ["randomFrom:to:", 1, 6], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -1000, 1000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -3000, 3000], 1000], "parameters"],
						["append:toList:", ["randomFrom:to:", 2, 7], "parameters"],
						["append:toList:",
							["*",
								["randomFrom:to:", 2, 7],
								["-", ["*", ["randomFrom:to:", 0, 1], 2], 1]],
							"parameters"],
						["append:toList:",
							["\/",
								["\/", ["\/", ["randomFrom:to:", 1000, 4000], 1000], ["getLine:ofList:", 86, "parameters"]],
								2],
							"parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -3000, 3000], 1000], "parameters"],
						["append:toList:", ["randomFrom:to:", 1, 6], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -500, 500], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -1000, 1000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -1000, 1000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -1000, 1000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", 0, 4000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", 500, 1500], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", 0, 4000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", 500, 1500], 1000], "parameters"],
						["append:toList:", ["randomFrom:to:", 3, 6], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", 100, 800], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -1000, 1000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -1000, 1000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", 500, 1000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -1000, 1000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -1000, 1000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -1000, 1000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -1000, 1000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -1000, 1000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -1000, 1000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -1000, 1000], 1000], "parameters"],
						["append:toList:", ["\/", ["randomFrom:to:", -1000, 1000], 1000], "parameters"]]
		  */

	
/*	,

	Script([
		
		
		
		V.blah << V.parameter, 
		V.blah <<  V.blah,
		Call("suma", [ V.blah, 33.3, S.zmienna_testowa ]),
		V.blah << V.zmienna_testowa,
		
		IfElse(
			EQ(3.0,5.0) & LT(V.par1,33.3), 
			[V.blah << Sin(V.par1*V.par2)],
			[V.ttt << Cos(V.par1/V.par2),
				V.par1 << V.par2,
				V.par2 << V.par1
			]
		)

	])
*/	
	
])
