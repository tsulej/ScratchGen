package pl.tsl.builder

class FractalFlameEval extends FFVars {

	def test = V.test << Rnd01
	
}

def e = new FractalFlameEval()

println e.test.eval()
println Var.varvals