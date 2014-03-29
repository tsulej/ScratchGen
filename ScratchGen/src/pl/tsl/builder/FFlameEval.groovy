package pl.tsl.builder

class FractalFlameEval extends FFVars {

	def test = Pow10(3.0)
	
}

def e = new FractalFlameEval()

println e.test.eval()