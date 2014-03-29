package pl.tsl.builder

class FFVars extends GenerateScratch {

// do not display below variables, they are already defined
	void init() {
		VariableG.varSet.varIgnore += ['x','y','r','r2','par1','par2','par3','par4','par5','par6','par7','atan2xy','atan2yx','_a','_b','_c','_d','_e','_f','_weight', 'temp_x','temp_y'] as Set
	}
		
	def fractal_flame = false // determine which script generate (difference in precalculations, eg. preblur

	def precalcFlame = {
		if(fractal_flame) {
			return [
				V.temp_x << V.temp_x + V._weight * V.x,
				V.temp_y << V.temp_y + V._weight * V.y,
				V.x << 0.0,
				V.y << 0.0
			]
		} else {
		return []
		}
	}

	def truchetFlame = {
		if(fractal_flame) {
			return [
				V.x << V.x / V._weight,
				V.y << V.y / V._weight
			]
		} else return []
	}

	def circlecropFlame = {
		if(fractal_flame) { [
			V.temp_x << 0,
			V.temp_y << 0,  ]
		} else return []
	}
	
	def flFunctions =  { [
	
		Def("pow", 'power %n ^ %n into %m.var',['X','A','VAR'],
			[ V.set( Par.VAR, Exp( Par.A * Ln( Par.X))) ]
		),

		Def("sinh", 'sinh %n into %m.var',['X','VAR'], // Result in Radians
			[ V.set( Par.VAR, 0.5 * ( Exp(Par.X) - Exp( 0.0 - Par.X)))]
		),
	
		Def("cosh", 'cosh %n into %m.var',['X','VAR'], // Result in Radians
			[ V.set( Par.VAR, 0.5 * ( Exp(Par.X) + Exp( 0.0 - Par.X)))]
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
] }


	def flCalc0_49 =  {  [
	
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
	]),


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
	)]
	}

	def flCalc50_99 = {  [Def("calc_50_59", 'calculate 50-59 %n %n %n',['func','X','Y'],
	[
		IfElse( EQ( Par.func,50), // Supershape
			[ V.par1 << M_PI_4 * R2D + V._supershape_pm_4 * V.atan2yx,
			  Call("pow",[Abs(Cos(V.par1)),V._supershape_n2, S.par3]),
			  Call("pow",[Abs(Sin(V.par1)),V._supershape_n3, S.par4]),
			  Call("pow",[ V.par3 + V.par4,V._supershape_pneg1_n1, S.par5]),
			  V.par2 << ((V._supershape_rnd * Rnd01 + (1.0 - V._supershape_rnd) * V.r) - V._supershape_holes) * V.par5 / V.r,
			  V.x << V.par2 * Par.X,
			  V.y << V.par2 * Par.Y
			],
			[IfElse ( EQ( Par.func, 51), // Flower
				[ V.par1 << (Rnd01 - V._flower_holes) * Cos(V._flower_petals * V.atan2yx) / V.r,
				  V.x << V.par1 * Par.X,
				  V.y << V.par1 * Par.Y
				],
				[IfElse ( EQ( Par.func, 52), // Conic
					[ V.par1 << (Rnd01 - V._conic_holes) * V._conic_eccentricity / (1.0 + V._conic_eccentricity * (Par.X / V.r)) / V.r,
					  V.x << V.par1 * Par.X,
					  V.y << V.par1 * Par.Y
					],
					[IfElse ( EQ( Par.func, 53), // Parabola
						[ V.par1 << Sin(R2D * V.r),
						  V.x << V._parabola_height * V.par1 * V.par1 * Rnd01,
						  V.y << V._parabola_weight * Cos(R2D * V.r) * Rnd01
						],
						[IfElse ( EQ( Par.func, 54), // Bent2
							[ IfElse( LT(Par.X, 0), [ V.par1 << Par.X * V._bent2_x ],[ V.par1 << Par.X] ),
							  IfElse( LT(Par.Y, 0), [ V.par2 << Par.Y * V._bent2_y ],[ V.par2 << Par.Y] ),
							  V.x << V.par1,
							  V.y << V.par2
							],
							[IfElse ( EQ( Par.func, 55), // Bipolar
								[ V.par1 << V.r2 + 1.0,
								  V.par2 << Par.X + Par.X,
								  Call("atan2",[2.0 * Par.Y, V.r2 - 1.0, S.par3]),
								  V.par3 << 0.5 * D2R * V.par3 + V._bipolar_shift,
								  IfElse( GT(V.par3, M_PI_2), [V.par3 << -M_PI_2 + (V.par3 +M_PI_2) % M_PI],
									[If ( LT(V.par3, -M_PI_2), [ V.par3 << M_PI_2 - (M_PI_2 - V.par3) % M_PI ])]
								  ),
								  V.x << 0.25 * M_2_PI * Ln( (V.par1 + V.par2) / (V.par1 - V.par2)),
								  V.y << M_2_PI * V.par3
								],
								[IfElse ( EQ(Par.func, 56), // Boarders
									[ V.par1 << Round(Par.X),
									  V.par2 << Round(Par.Y),
									  V.par3 << Par.X - V.par1,
									  V.par4 << Par.Y - V.par2,
									  IfElse( GT( Rnd01, 0.75),
										  [ V.x << 0.5 * V.par3 + V.par1,
											V.y << 0.5 * V.par4 + V.par2
										  ],
										  [ IfElse( ~(LT(Abs(V.par3),Abs(V.par4))),
											  [ IfElse( LT(V.par3,0),
												  [ V.x << 0.5 * V.par3 + V.par1 - 0.25,
													V.y << 0.5 * V.par4 + V.par2 - 0.25 * V.par4 / V.par3
												  ],
												  [ V.x << 0.5 * V.par3 + V.par1 + 0.25,
													V.y << 0.5 * V.par4 + V.par2 + 0.25 * V.par4 / V.par3
												  ]
												)
											  ],
											  [ IfElse( LT(V.par4,0),
												  [ V.y << 0.5 * V.par4 + V.par2 - 0.25,
													V.x << 0.5 * V.par3 + V.par1 - V.par3 / V.par4 * 0.25
												  ],
												  [ V.y << 0.5 * V.par4 + V.par2 + 0.25,
													V.x << 0.5 * V.par3 + V.par1 + V.par3 / V.par4 * 0.25
												  ]
												)
											  ])
										  ]
									  )
									],
									[IfElse ( EQ( Par.func, 57), // Butterfly
										[ V.par1 << Par.Y + Par.Y,
										  V.par2 << 1.3029400317411197908970256609023 * Sqrt(Abs(Par.Y * Par.X)/(Par.X * Par.X + V.par1 * V.par1)),
										  V.x << V.par2 * Par.X,
										  V.y << V.par2 * V.par1
										],
										[IfElse ( EQ( Par.func, 58), // Cell
											[ V.par1 << Floor(Par.X / V._cell_size),
											  V.par2 << Floor(Par.Y / V._cell_size),
											  V.par3 << Par.X - V.par1 * V._cell_size,
											  V.par4 << Par.Y - V.par2 * V._cell_size,
											  IfElse( LT(V.par2,0),
												  [ IfElse( LT(V.par1,0),
													  [ V.par2 << -(2.0 * V.par2 + 1.0), V.par1 << -(2.0 * V.par1 + 1.0) ],
													  [ V.par2 << -(2.0 * V.par2 + 1.0), V.par1 << 2.0 * V.par1]
												  )],
												  [ IfElse( LT(V.par1,0),
													  [ V.par2 << 2.0 * V.par2, V.par1 << -(2.0 * V.par1 + 1.0)],
													  [ V.par2 << 2.0 * V.par2, V.par1 << 2.0 * V.par1]
												  )]
											  ),
											  V.x << V.par3 + V.par1 * V._cell_size,
											  V.y << -(V.par4 + V.par2 * V._cell_size)
											],
											[ V.par1 << 0.5 * Ln(V.r2), // CPow
											  V.par2 << R2D * (V._cpow_r * D2R * V.atan2yx + V._cpow_i * V.par1 + V._cpow_va * Floor(V._cpow_power * Rnd01)),
											  V.par3 << Exp(V._cpow_r * V.par1 - V._cpow_i * D2R * V.atan2yx),
											  V.x << V.par3 * Cos(V.par2),
											  V.y << V.par3 * Sin(V.par2)
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

	Def("calc_60_69", 'calculate 60-69 %n %n %n',['func','X','Y'],
	[
		IfElse( EQ( Par.func,60), // Curve
			[ V.x << Par.X + V._curve_xamp * Exp( -(Par.Y) * Par.Y / V._curve_xlength),
			  V.y << Par.Y + V._curve_yamp * Exp( -(Par.X) * Par.X / V._curve_ylength)
			],
			[IfElse ( EQ( Par.func, 61), // Edisc
				[ V.par1 << V.r2 + 1.0,
				  V.par2 << Par.X + Par.X,
				  V.par3 << 0.5 * (Sqrt(V.par1 + V.par2) + Sqrt(V.par1 - V.par2)), //xmax
				  V.par1 << R2D * Ln(V.par3 + Sqrt(V.par3 - 1.0)), //a1
				  V.par2 << D2R * -(ACos(Par.X / V.par3)), //a2
				  Call("cosh",[V.par2, S.par3]), // cshu
				  Call("sinh",[V.par2, S.par4]), // snhu
				  V.par5 << Sin(V.par1),
				  If( GT(Par.Y,0.0), [V.par5 << -(V.par5)]),
				  V.x << V.par3 * Cos(V.par1) / 11.57034632,
				  V.y << V.par4 * V.par5 / 11.57034632
				],
				[IfElse ( EQ( Par.func, 62), // Elliptic
					[ V.par1 << V.r2 + 1.0,
					  V.par2 << Par.X + Par.X,
					  V.par3 << 0.5 * (Sqrt(V.par1 + V.par2) + Sqrt(V.par1 - V.par2)), //xmax
					  V.par1 << Par.X / V.par3, // a
					  V.par2 << 1.0 - V.par1 * V.par1, // b
					  V.par5 << V.par3 - 1.0, // ssx
					  IfElse( LT(V.par2,0),[V.par2 << 0.0], [V.par2 << Sqrt(V.par2)]),
					  IfElse( LT(V.par5,0),[V.par5 << 0.0], [V.par5 << Sqrt(V.par5)]),
					  Call("atan2",[V.par1, V.par2, S.par4]),
					  V.x << (D2R / M_PI_2) * V.par4,
					  IfElse( GT(Par.Y,0),
						  [ V.y << Ln(V.par3 + V.par5)],
						  [ V.y << -(Ln(V.par3 + V.par5))]
					  )
					],
					[IfElse ( EQ( Par.func, 63), // Esher
						[ V.par1 << D2R * V.atan2yx, // a
						  V.par2 << 0.5 * Ln(V.r2), // lnr
						  V.par3 << Exp(V._esher_vc * V.par2 - V._esher_vd * V.par1), //m
						  V.par4 << R2D * (V._esher_vc * V.par1 + V._esher_vd * V.par2),//n
						  V.x << V.par3 * Cos(V.par4),
						  V.y << V.par3 * Sin(V.par4)
						],
						[IfElse ( EQ( Par.func, 64), // Foci
							[ V.par1 << 0.5 * Exp(Par.X),
							  V.par2 << 0.25 / V.par1,
							  V.par3 << R2D * Par.Y,
							  V.par4 << 1.0 / (V.par1 + V.par2 - Cos(V.par3)),
							  V.x << V.par4 * (V.par1 - V.par2),
							  V.y << V.par4 * Sin(V.par3)
							],
							[IfElse ( EQ( Par.func, 65), // Lazysuzan
								[ V.par1 << Par.X - V._lazysusan_x, //x
								  V.par2 << Par.Y + V._lazysusan_y, //y
								  V.par3 << Sqrt(V.par1*V.par1 + V.par2*V.par2), // r
								  IfElse( LT(V.par3, V._weight),
									  [ Call("atan2",[V.par2,V.par1,S.par4]),
										V.par5 << V.par4 + V._lazysusan_spin + V._lazysusan_twist * (V._weight - V.par3),
										V.x << V.par3 * Cos(V.par5) + V._lazysusan_x / V._weight,
										V.y << V.par3 * Sin(V.par5) - V._lazysusan_y / V._weight
									  ],
									  [ V.par5 << 1.0 + V._lazysusan_space / V.par3,
										V.x << V.par5 * V.par1 + V._lazysusan_x / V._weight,
										V.y << V.par5 * V.par2 - V._lazysusan_y / V._weight
									  ]
								  )
								],
								[IfElse ( EQ(Par.func, 66), // Loonie
									[ V.par1 << V._weight * V._weight,
									  IfElse( LT(V.r2, V.par1),
										  [ V.par2 << Sqrt(V.par1 / V.r2 - 1.0),
											V.x << V.par2 * Par.X,
											V.y << V.par2 * Par.Y
										  ],
										  [ V.x << Par.X,
											V.y << Par.Y
										  ]
									  )
									],
									[IfElse ( EQ( Par.func, 67), // Preblur
										[ V.par1 << Rnd01 + Rnd01 + Rnd01 + Rnd01 - 2.0,
										  V.par2 << 2.0 * M_PI * R2D * Rnd01,
										  V.x << V.par1 * Cos(V.par2),
										  V.y << V.par1 * Sin(V.par2),
										] + precalcFlame(),
										[IfElse ( EQ( Par.func, 68), // Modulus
											[ V.par1 << 2.0 * V._modulus_x,
											  V.par2 << 2.0 * V._modulus_y,
											  IfElse( GT(Par.X, V._modulus_x),
												  [ V.x << -(V._modulus_x) + (Par.X + V._modulus_x) % V.par1 ],
												  [ IfElse( LT(Par.X, -(V._modulus_x)),
													  [ V.x << V._modulus_x - (V._modulus_x - Par.X) % V.par1],
													  [ V.x << Par.X]
												  )]
											  ),
												IfElse( GT(Par.Y, V._modulus_y),
												[ V.y << -(V._modulus_y) + (Par.Y + V._modulus_y) % V.par2 ],
												[ IfElse( LT(Par.Y, -(V._modulus_y)),
												  [ V.y << V._modulus_y - (V._modulus_y - Par.Y) % V.par2],
												  [ V.y << Par.Y]
												)]
											  )
											],
											[ IfElse( LT(V._oscilloscope_damping,0.1),  // Oscilloscope
												[ V.par1 << V._oscilloscope_amplitude * Cos(V._oscilloscope_frequency * Par.X) + V._oscilloscope_separation ],
												[ V.par1 << V._oscilloscope_amplitude * Exp(-(Abs(Par.X))*V._oscilloscope_damping) * Cos(V._oscilloscope_frequency * Par.X) + V._oscilloscope_separation ]
											  ),
											  V.x << Par.X,
											  IfElse( GT(Abs(Par.Y),V.par1),[ V.y << -(Par.Y)], [ V.y << Par.Y])
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

	Def("calc_70_79", 'calculate 70-79 %n %n %n',['func','X','Y'],
	[
		IfElse( EQ( Par.func, 70), // Polar2
			[ V.x << D2R * (1.0/M_PI) * V.atan2xy,
			  V.y << (1.0/M_PI)/2.0 * Ln(V.r2)
			],
			[IfElse ( EQ( Par.func, 71), // Popcorn2
				[ V.x << Par.X + V._popcorn2_x * Sin(R2D * Tan(Par.Y * V._popcorn2_c)),
				  V.y << Par.Y + V._popcorn2_y * Sin(R2D * Tan(Par.X * V._popcorn2_c)),
				],
				[IfElse ( EQ( Par.func, 72), // Scry
					[ V.par1 << (1.0 / (V.r * (V.r2 + 1.0/V._weight))) / V._weight,
					  V.x << Par.X * V.par1,
					  V.y << Par.Y * V.par1
					],
					[IfElse ( EQ( Par.func, 73), // Separation
						[ IfElse ( GT(Par.X,0.0),
							[V.x << Sqrt(Par.X * Par.X + V._separation_x) - Par.X * V._separation_xinside],
							[V.x << -1.0 * (Sqrt(Par.X * Par.X + V._separation_x) + Par.X * V._separation_xinside)]
						  ),
							IfElse ( GT(Par.Y,0.0),
							[V.y << Sqrt(Par.Y * Par.Y + V._separation_y) - Par.Y * V._separation_yinside],
							[V.y << -1.0 * (Sqrt(Par.Y * Par.Y + V._separation_y) + Par.Y * V._separation_yinside)]
						  )
						],
						[IfElse ( EQ( Par.func, 74), // Split
							[ IfElse( GT(Cos(M_PI*R2D*Par.X*V._split_xsize),0.0),
								[V.y << Par.Y],
								[V.y << -(Par.Y)]
							  ),
								IfElse( GT(Cos(M_PI*R2D*Par.Y*V._split_ysize),0.0),
								[V.x << Par.X],
								[V.x << -(Par.X)]
							  )
							],
							[IfElse ( EQ( Par.func, 75), // Splits
								[ IfElse( GT(Par.X ,0.0),
									[ V.x << Par.X + V._splits_x ],
									[ V.x << Par.X - V._splits_x ]
								  ),
									IfElse( GT(Par.Y ,0.0),
									[ V.y << Par.Y + V._splits_y ],
									[ V.y << Par.Y - V._splits_y ]
								  )
								],
								[IfElse ( EQ(Par.func, 76), // Stripes
									[ V.par1 << Floor(Par.X + 0.5), //roundx
									  V.par2 << Par.X - V.par1, //offsetx
									  V.x << V.par2 * V._stripes_space + V.par1,
									  V.y << Par.Y + V.par2 * V.par2 * V._stripes_warp
									],
									[IfElse ( EQ( Par.func, 77), // Wedge
										[ V.par1 << D2R * V.atan2yx + V._wedge_swirl * V.r, // a
										  V.par2 << Floor(M_1_PI * 0.5 * (V._wedge_count * V.par1 + M_PI)), //c
										  V.par1 << R2D * (V.par1 * (1.0 - M_1_PI * 0.5 * V._wedge_angle * V._wedge_count) + V.par2 * V._wedge_angle),
										  V.par2 << V.r + V._wedge_hole,
										  V.x << V.par2 * Cos(V.par1),
										  V.y << V.par2 * Sin(V.par1)
										],
										[IfElse ( EQ( Par.func, 78), // WedgeJulia
											[ Call("pow",[V.r2, V._wedgejulia_cn, S.par1]), // r
											  V.par2 << (D2R * V.atan2yx + 2.0 * M_PI * Round(V._wedgejulia_power * Rnd01)) / V._wedgejulia_power, //a
											  V.par3 << Floor( M_1_PI * 0.5 * (V._wedgejulia_count * V.par2 + M_PI)),
											  V.par2 << R2D * (V.par2 * V._wedgejulia_cf + V.par3 * V._wedgejulia_angle),
											  V.x << V.par1 * Cos(V.par2),
											  V.y << V.par1 * Sin(V.par2)
											],
											[ V.par1 << 1.0 / V.r, //r
											  V.par2 << D2R * V.atan2yx + V._wedgesph_swirl * V.par1, //a
											  V.par3 << Floor( M_1_PI * 0.5 * (V._wedgesph_count * V.par2 + M_PI)),
											  V.par2 << R2D * (V.par2 * (1.0 - 0.5*M_1_PI*V._wedgesph_angle*V._wedgesph_count) + V.par3 * V._wedgesph_angle),
											  V.par1 << V.par1 + V._wedgesph_hole,
											  V.x << V.par1 * Cos(V.par2),
											  V.y << V.par1 * Sin(V.par2),
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

	Def("calc_80_89", 'calculate 80-89 %n %n %n',['func','X','Y'],
	[
		IfElse( EQ( Par.func,80), // Whorl
			[ IfElse( LT(V.r, V._weight),
				[ V.par1 << V.atan2yx + R2D * V._whorl_inside/(V._weight - V.r) ],
				[ V.par1 << V.atan2yx + R2D * V._whorl_outside/(V._weight - V.r)]
			  ),
			  V.x << V.r * Cos(V.par1),
			  V.y << V.r * Sin(V.par1)
			],
			[IfElse ( EQ( Par.func, 81), // Waves2
				[ V.x << Par.X + V._waves2_scalex * Sin(R2D * Par.Y * V._waves2_freqx),
				  V.y << Par.Y + V._waves2_scaley * Sin(R2D * Par.X * V._waves2_freqy)
				],
				[IfElse ( EQ( Par.func, 82), // Exp
					[ V.par1 << Exp(Par.X),
					  V.par2 << R2D * Par.Y,
					  V.x << V.par1 * Cos(V.par2),
					  V.y << V.par1 * Sin(V.par2)
					],
					[IfElse ( EQ( Par.func, 83), // Log
						[ V.x << 0.5 * Ln(V.r2),
						  V.y << D2R * V.atan2yx
						],
						[IfElse ( EQ( Par.func, 84), // Sin
							[ V.par1 << R2D * Par.X,
							  Call("sinh",[Par.Y, S.par2]),
							  Call("cosh",[Par.Y, S.par3]),
							  V.x << Sin(V.par1) * V.par3,
							  V.y << Cos(V.par1) * V.par2
							],
							[IfElse ( EQ( Par.func, 85), // Cos
								[ V.par1 << R2D * Par.X,
								  Call("sinh",[Par.Y, S.par2]),
								  Call("cosh",[Par.Y, S.par3]),
								  V.x << Cos(V.par1) * V.par3,
								  V.y << -(Sin(V.par1) * V.par2)
								],
								[IfElse ( EQ(Par.func, 86), // Tan
									[ V.par1 << R2D * 2.0 * Par.X,
									  Call("sinh",[2.0 * Par.Y, S.par2]),
									  Call("cosh",[2.0 * Par.Y, S.par3]),
									  V.par4 << 1.0 / ( Cos(V.par1) + V.par3),
									  V.x << V.par4 * Sin(V.par1),
									  V.y << V.par4 * V.par2
									],
									[IfElse ( EQ( Par.func, 87), // Sec
										[ V.par1 << R2D * Par.X,
										  Call("sinh",[Par.Y, S.par2]),
										  Call("cosh",[Par.Y, S.par3]),
										  Call("cosh",[2.0 * Par.Y,S.par5]),
										  V.par4 << 2.0 / (Cos(2.0 *V.par1) + V.par5),
										  V.x << V.par4 * Cos(V.par1) * V.par3,
										  V.y << V.par4 * Sin(V.par1) * V.par2
										],
										[IfElse ( EQ( Par.func, 88), // Csc
											[ V.par1 << R2D * Par.X,
											  Call("sinh",[Par.Y, S.par2]),
											  Call("cosh",[Par.Y, S.par3]),
											  Call("cosh",[2.0 * Par.Y,S.par5]),
											  V.par4 << 2.0 / (V.par5 - Cos(2.0 *V.par1)),
											  V.x << V.par4 * Sin(V.par1) * V.par3,
											  V.y << -(V.par4 * Cos(V.par1) * V.par2)
											],
											[ V.par1 << R2D * 2.0 * Par.X, // Cot
											  Call("sinh",[2.0 * Par.Y, S.par2]),
											  Call("cosh",[2.0 * Par.Y, S.par3]),
											  V.par4 << 1.0 / ( V.par3 - Cos(V.par1)),
											  V.x << V.par4 * Sin(V.par1),
											  V.y << -(V.par4 * V.par2)
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

Def("calc_90_99", 'calculate 90-99 %n %n %n',['func','X','Y'],
	[
		IfElse( EQ( Par.func,90), // Sinh
			[ V.par1 << R2D * Par.Y,
			  Call("sinh",[Par.X, S.par2]),
			  Call("cosh",[Par.X, S.par3]),
			  V.x << Cos(V.par1) * V.par2,
			  V.y << Sin(V.par1) * V.par3
			],
			[IfElse ( EQ( Par.func, 91), // Cosh
				[ V.par1 << R2D * Par.Y,
				  Call("sinh",[Par.X, S.par2]),
				  Call("cosh",[Par.X, S.par3]),
				  V.x << Cos(V.par1) * V.par3,
				  V.y << Sin(V.par1) * V.par2
				],
				[IfElse ( EQ( Par.func, 92), // Tanh
					[ V.par1 << R2D * 2.0 * Par.Y,
					  Call("sinh",[2.0 * Par.X, S.par2]),
					  Call("cosh",[2.0 * Par.X, S.par3]),
					  V.par4 << 1.0 / ( Cos(V.par1) + V.par3),
					  V.x << V.par4 * V.par2,
					  V.y << V.par4 * Sin(V.par1)
					],
					[IfElse ( EQ( Par.func, 93), // Sech
						[ V.par1 << R2D * Par.Y,
						  Call("sinh",[Par.X, S.par2]),
						  Call("cosh",[Par.X, S.par3]),
						  Call("cosh",[2.0 * Par.X,S.par5]),
						  V.par4 << 2.0 / (Cos(2.0 *V.par1) + V.par5),
						  V.x << V.par4 * Cos(V.par1) * V.par3,
						  V.y << -(V.par4 * Sin(V.par1) * V.par2)
						],
						[IfElse ( EQ( Par.func, 94), // Csch
							[ V.par1 << R2D * Par.Y,
							  Call("sinh",[Par.X, S.par2]),
							  Call("cosh",[Par.X, S.par3]),
							  Call("cosh",[2.0 * Par.X, S.par5]),
							  V.par4 << 2.0 / (V.par5 - Cos(2.0 *V.par1)),
							  V.x << V.par4 * Cos(V.par1) * V.par2,
							  V.y << -(V.par4 * Sin(V.par1) * V.par3)
							],
							[IfElse ( EQ( Par.func, 95), // Coth
								[ V.par1 << R2D * 2.0 * Par.Y,
								  Call("sinh",[2.0 * Par.X, S.par2]),
								  Call("cosh",[2.0 * Par.X, S.par3]),
								  V.par4 << 1.0 / ( V.par3 - Cos(V.par1)),
								  V.x << V.par4 * V.par2,
								  V.y << V.par4 * Sin(V.par1)
								],
								[IfElse ( EQ(Par.func, 96), // Auger
									[ V.par1 << Sin(V._auger_freq * Par.X), //s
									  V.par2 << Sin(V._auger_freq * Par.Y), //t
									  V.par3 << V._auger_weight * (V._auger_scale * V.par2 / 2.0 + Abs(Par.X) * V.par2), // dx
									  V.x << Par.X + V._auger_sym * V.par3,
									  V.y << Par.Y + V._auger_weight * (V._auger_scale * V.par1 / 2.0 + Abs(Par.Y) * V.par1)
									],
									[IfElse ( EQ( Par.func, 97), // Flux
										[ V.par1 << Par.X + V._weight, // xpw
										  V.par2 << Par.X - V._weight, // xmw
										  V.par3 << V._flux_spread * Sqrt (Sqrt (Par.Y * Par.Y + V.par1 * V.par1) / Sqrt(Par.Y * Par.Y + V.par2 * V.par2)),
										  Call("atan2",[Par.Y, V.par2, S.par4]),
										  Call("atan2",[Par.Y, V.par1, S.par5]),
										  V.par1 << 0.5 * (V.par4 - V.par5),
										  V.x << V.par3 * Cos(V.par1),
										  V.y << V.par3 * Sin(V.par1)
										],
										[IfElse ( EQ( Par.func, 98), // Mobius
											[ V.par1 << V._mobius_re_a * Par.X - V._mobius_im_a * Par.Y + V._mobius_re_b, // re_u
											  V.par2 << V._mobius_re_a * Par.Y + V._mobius_im_a * Par.X + V._mobius_im_b, // im_u
											  V.par3 << V._mobius_re_c * Par.X - V._mobius_im_c * Par.Y + V._mobius_re_d, // re_v
											  V.par4 << V._mobius_re_c * Par.Y + V._mobius_im_c * Par.X + V._mobius_im_d, // im_v
											  V.par5 << 1.0 / (V.par3 * V.par3 + V.par4 * V.par4),
											  V.x << V.par5 * (V.par1 * V.par3 + V.par2 * V.par4),
											  V.y << V.par5 * (V.par1 * V.par3 - V.par1 * V.par4)
											],
											[ // Truchet
											  V.par1 << V._truchet_scale * Par.X / V._weight,
											  V.par2 << V._truchet_scale * Par.Y / V._weight,
											  V.par3 << V.par1 - Round(V.par1),
											  IfElse( LT(V.par3,0), [ V.par1 << 1.0 + V.par3], [ V.par1 << V.par3 ] ),
											  V.par3 << V.par2 - Round(V.par2),
											  IfElse( LT(V.par3,0), [ V.par2 << 1.0 + V.par3], [ V.par2 << V.par3 ] ),
											  // Par3 == Tiletype
											  V.par4 << Round(Par.X) * V._truchet_seed2,
											  V.par5 << Round(Par.Y) * V._truchet_seed2,
											  V.par4 << (V.par4 + V.par5 + V.par4 * V.par5 + V._truchet_seed) * V._truchet_seed2 / 2.0,
											  V.par4 << (V.par4 * 32747.0 + 12345.0) % 65535.0,
											  IfElse ( LT(V.par4 % 4.0,2.0),
												  [ Call("pow",[Abs(V.par1),V._truchet_exponent, S.par3]),
													Call("pow",[Abs(V.par2),V._truchet_exponent, S.par4]),
													Call("pow",[V.par3 + V.par4,V._truchet_onen, S.par5]),
													V.par5 << Abs(V.par5 - 0.5) / V._truchet_rmax, // r0
													Call("pow",[Abs(V.par1-1.0),V._truchet_exponent, S.par3]),
													Call("pow",[Abs(V.par2-1.0),V._truchet_exponent, S.par4]),
													Call("pow",[V.par3 + V.par4,V._truchet_onen, S.par4]),
													V.par4 << Abs(V.par4 - 0.5) / V._truchet_rmax // r1
												  ],
												  [ Call("pow",[Abs(V.par1-1.0),V._truchet_exponent, S.par3]),
													Call("pow",[Abs(V.par2),V._truchet_exponent, S.par4]),
													Call("pow",[V.par3 + V.par4,V._truchet_onen, S.par5]),
													V.par5 << Abs(V.par5 - 0.5) / V._truchet_rmax, // r0
													Call("pow",[Abs(V.par1),V._truchet_exponent, S.par3]),
													Call("pow",[Abs(V.par2-1.0),V._truchet_exponent, S.par4]),
													Call("pow",[V.par3 + V.par4,V._truchet_onen, S.par4]),
													V.par4 << Abs(V.par4 - 0.5) / V._truchet_rmax, // r1
												  ]
											  ),
											  V.x << 0.0,
											  V.y << 0.0,
											  If( LT(V.par5, 1.0),
												  [ V.x << V._truchet_size * (V.par1 + Floor(Par.X)),
													V.y << V._truchet_size * (V.par2 + Floor(Par.Y))
												  ]
											  ),
												If( LT(V.par4, 1.0),
													[ V.x << V.x + V._truchet_size * (V.par1 + Floor(Par.X)),
													  V.y << V.y + V._truchet_size * (V.par2 + Floor(Par.Y))
													]
											  )
											] + truchetFlame()
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
	)
] }


	def flCalc100_109 = { [
	
	Def("calc_100_109", 'calculate 100-109 %n %n %n',['func','X','Y'],
		[
			IfElse( EQ( Par.func, 100), // BCollide
				[ V.par1 << Par.X + 1.0,
				  V.par2 << Par.X - 1.0,
				  V.par3 << 0.5 * (Ln(V.par1 * V.par1 + Par.Y * Par.Y) - Ln(V.par2 * V.par2 + Par.Y * Par.Y)), // tau
				  Call("atan2",[Par.Y, V.par1, S.par1]),
				  Call("atan2",[Par.Y, -(V.par2), S.par2]),
				  V.par4 << M_PI - D2R * V.par1 - D2R * V.par2, // sigma
				  V.par5 << Floor(V.par4 * V._bcollide_bcn_pi),
				  IfElse( EQ(V.par5 % 2,0),
					  [ V.par4 << V.par5 * V._bcollide_pi_bcn + ( (V.par4 + V._bcollide_bca_bcn) % V._bcollide_pi_bcn )],
					  [ V.par4 << V.par5 * V._bcollide_pi_bcn + ( (V.par4 - V._bcollide_bca_bcn) % V._bcollide_pi_bcn )]
				  ),
				  V.par4 << R2D * V.par4,
				  Call("sinh",[V.par3, S.par1]),
				  Call("cosh",[V.par3, S.par2]),
				  V.par5 << V.par2 - Cos(V.par4),
				  V.x << V.par1 / V.par5,
				  V.y << Sin(V.par4) / V.par5
				],
				[IfElse ( EQ( Par.func, 101), // BMod
					[ V.par1 << Par.X + 1.0,
					  V.par2 << Par.X - 1.0,
					  V.par3 << 0.5 * (Ln(V.par1 * V.par1 + Par.Y * Par.Y) - Ln(V.par2 * V.par2 + Par.Y * Par.Y)), // tau
					  Call("atan2",[Par.Y, V.par1, S.par1]),
					  Call("atan2",[Par.Y, -(V.par2), S.par2]),
					  V.par4 << M_PI - D2R * V.par1 - D2R * V.par2, // sigma
					  If( LT( V.par3, V._bmod_radius) & LT(-(V.par3),V._bmod_radius),
						  [ V.par3 << (V.par3 + V._bmod_radius + V._bmod_distance * V._bmod_radius) % (2.0 * V._bmod_radius) - V._bmod_radius]
					  ),
				  	  V.par4 << R2D * V.par4,
					  Call("sinh",[V.par3, S.par1]),
					  Call("cosh",[V.par3, S.par2]),
					  V.par5 << V.par2 - Cos(V.par4),
					  V.x << V.par1 / V.par5,
					  V.y << Sin(V.par4) / V.par5
					],
					[IfElse ( EQ( Par.func, 102), // BSwirl
						[ V.par1 << Par.X + 1.0,
						  V.par2 << Par.X - 1.0,
						  V.par3 << 0.5 * (Ln(V.par1 * V.par1 + Par.Y * Par.Y) - Ln(V.par2 * V.par2 + Par.Y * Par.Y)), // tau
						  Call("atan2",[Par.Y, V.par1, S.par1]),
						  Call("atan2",[Par.Y, -(V.par2), S.par2]),
						  V.par4 << M_PI - D2R * V.par1 - D2R * V.par2, // sigma
						  V.par4 << R2D * (V.par4 + V.par3 * V._bswirl_out + V._bswirl_in / V.par3),
						  Call("sinh",[V.par3, S.par1]),
						  Call("cosh",[V.par3, S.par2]),
						  V.par5 << V.par2 - Cos(V.par4),
						  V.x << V.par1 / V.par5,
						  V.y << Sin(V.par4) / V.par5
						],
						[IfElse ( EQ( Par.func, 103), // BTransform
							[ V.par1 << Par.X + 1.0,
							  V.par2 << Par.X - 1.0,
							  V.par3 << 0.5 * (Ln(V.par1 * V.par1 + Par.Y * Par.Y) - Ln(V.par2 * V.par2 + Par.Y * Par.Y)) / V._btransform_power + V._btransform_move, // tau
							  Call("atan2",[Par.Y, V.par1, S.par1]),
							  Call("atan2",[Par.Y, -(V.par2), S.par2]),
							  V.par4 << M_PI - D2R * V.par1 - D2R * V.par2 + V._btransform_rotate, // sigma
							  V.par4 << R2D * (V.par4 / V._btransform_power + M_2PI / V._btransform_power * Floor(Rnd01 * V._btransform_power) ),
							  IfElse( GT(Par.X, 0),
								  [V.par3 << V.par3 + V._btransform_split],
								  [V.par3 << V.par3 - V._btransform_split]
							  ),
						  	  Call("sinh",[V.par3, S.par1]),
							  Call("cosh",[V.par3, S.par2]),
							  V.par5 << V.par2 - Cos(V.par4),
							  V.x << V.par1 / V.par5,
							  V.y << Sin(V.par4) / V.par5
							],
							[IfElse ( EQ( Par.func, 104), // BWraps7
								[ V.par1 << (Floor(Par.X / V._bwraps7_cellsize) + 0.5) * V._bwraps7_cellsize, // Cx
								  V.par2 << (Floor(Par.Y / V._bwraps7_cellsize) + 0.5) * V._bwraps7_cellsize, // Cy
								  V.par3 << Par.X - V.par1, // Lx
								  V.par4 << Par.Y - V.par2, // Ly
								  IfElse( GT(V.par3 * V.par3 + V.par4 * V.par4, V._bwraps7_r2),
									  [ V.x << Par.X,
										V.y << Par.Y
									  ],
								      [ V.par3 << V.par3 * V._bwraps7_g2,
										V.par4 << V.par4 * V._bwraps7_g2,
										V.par5 << V._bwraps7_rfactor / ((V.par3 * V.par3 + V.par4 * V.par4) / 4.0 + 1.0),
										V.par3 << V.par3 * V.par5,
										V.par4 << V.par4 * V.par5,
										V.par5 << (V.par3 * V.par3 + V.par4 * V.par4) / V._bwraps7_r2, // r
										V.par6 << R2D * (V._bwraps7_inner_twist * (1.0 - V.par5) + V._bwraps7_outer_twist * V.par5),
										V.par7 << Sin(V.par6), // s
										V.par6 << Cos(V.par6), // c
										V.x << V.par1 + V.par6 * V.par3 + V.par7 * V.par4,
										V.y << V.par2 - V.par7 * V.par3 + V.par6 * V.par4,
										
									  ]
								  )
								],
								[IfElse ( EQ( Par.func, 105), // Barycentroid
									[ V.par1 << V._barycentroid_a * Par.X + V._barycentroid_b * Par.Y, // dot02
									  V.par2 << V._barycentroid_c * Par.X + V._barycentroid_d * Par.Y, // dot12
									  V.par3 << (V._barycentroid_dot11 * V.par1 - V._barycentroid_dot01 * V.par2) * V._barycentroid_invdenom,
									  V.par4 << (V._barycentroid_dot00 * V.par2 - V._barycentroid_dot01 * V.par1) * V._barycentroid_invdenom,
									  IfElse( LT(V.par3,0), [V.par1 << -1.0], [ IfElse( GT(V.par3,0), [V.par1 << 1.0], [V.par1 << 0.0]) ]), // Sign
									  IfElse( LT(V.par4,0), [V.par2 << -1.0], [ IfElse( GT(V.par4,0), [V.par2 << 1.0], [V.par2 << 0.0]) ]), // Sign
									  V.x << Sqrt(V.par3 * V.par3 + Par.X * Par.X) * V.par1,
									  V.y << Sqrt(V.par4 * V.par4 + Par.Y * Par.Y) * V.par2
									],
									[IfElse ( EQ(Par.func, 106), // Bilinear
										[ V.x << Par.Y,
										  V.y << Par.X,
										],
										[IfElse ( EQ( Par.func, 107), // BlockY
											[ V.par1 << V._weight / ((Cos(Par.X * R2D) + Cos(Par.Y * R2D)) / V._blocky_mp + 1.0), // r
											  V.par2 << V.r2 + 1.0, // tmp
											  V.par3 << 2.0 * Par.X,
											  V.par4 << 2.0 * Par.Y,
											  V.par3 << Par.X / (0.5 * ( Sqrt(V.par2  + V.par3) + Sqrt(V.par2 - V.par3) )), 
											  V.par4 << Par.Y / (0.5 * ( Sqrt(V.par2  + V.par4) + Sqrt(V.par2 - V.par4) )),
											  V.par5 << 1.0 - V.par3 * V.par3,
											  IfElse( LT(V.par5,0.0), [ V.par5 << 0.0], [V.par5 << Sqrt(V.par5)]), 
											  Call("atan2", [V.par3, V.par5, S.par3]),
											  V.x << (D2R / M_PI_2) * V.par3 * V.par1 * V._blocky_x,
											  V.par5 << 1.0 - V.par4 * V.par4,
											  IfElse( LT(V.par5,0.0), [ V.par5 << 0.0], [V.par5 << Sqrt(V.par5)]),
											  Call("atan2", [V.par4, V.par5, S.par4]),
											  V.y << (D2R / M_PI_2) * V.par4 * V.par1 * V._blocky_y
											],
											[IfElse ( EQ( Par.func, 108), // BlurZoom
												[ V.par1 << 1.0 + V._blurzoom_length * Rnd01,
												  V.x << (Par.X - V._blurzoom_x) * V.par1 + V._blurzoom_x,
												  V.y << (Par.Y - V._blurzoom_y) * V.par1 + V._blurzoom_y,
												],
												[ V.par1 << Round(Par.X), // roundX // Boarders2
												  V.par2 << Round(Par.Y), // roundY
												  V.par3 << Par.X - V.par1, // offsetX
												  V.par4 << Par.Y - V.par2, // offsetY 
												  IfElse( GT(Rnd01,V._boarders2_cr),
													  [ V.x << V.par3 * V._boarders2_c + V.par1,
														V.y << V.par4 * V._boarders2_c + V.par2,
													  ],
												   	  [ IfElse( GT(Abs(V.par3),Abs(V.par4)),
															 [ IfElse( GT(V.par3,0.0),
																 [ V.x << V.par3 * V._boarders2_c + V.par1 + V._boarders2_cl,
																   V.y << V.par4 * V._boarders2_c + V.par2 + V._boarders2_cl * V.par4/V.par3 
															     ],
															     [ V.x << V.par3 * V._boarders2_c + V.par1 - V._boarders2_cl,
																   V.y << V.par4 * V._boarders2_c + V.par2 - V._boarders2_cl * V.par4/V.par3
															     ]
															   )
														     ],
														     [ IfElse( GT(V.par4,0.0),
																 [ V.y << V.par4 * V._boarders2_c + V.par2 + V._boarders2_cl,
																   V.x << V.par3 * V._boarders2_c + V.par1 + V._boarders2_cl * V.par3/V.par4 
															     ],
															     [ V.y << V.par4 * V._boarders2_c + V.par2 - V._boarders2_cl,
																   V.x << V.par3 * V._boarders2_c + V.par1 - V._boarders2_cl * V.par3/V.par4
															     ]
															   )
														     ]
														)
													  ]
												  )
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
		)
]	
	



	/* template
	
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
	
	*/
 }

def flPrecalc110_119 = {
		[
			Script([
				// CannabisCurve
				V._cannabiscurve_filled << Rnd(0,1),
				// Checks
				V._checks_x << Rnd(0.2,1.0),
				V._checks_y << Rnd(0.2,1.0),
				V._checks_size << Rnd(0.4,1.4),
				V._checks_rnd << Rnd(0,1),
				// CircleCrop
				V._circlecrop_x << Rnd(-0.5,0.5),
				V._circlecrop_y << Rnd(-0.5,0.5),
				V._circlecrop_scatter_area << Rnd(-0.99,0.99),
				V._circlecrop_zero << Rnd(0,1),
				V._circlecrop_radius << Rnd(0.3,1.0),
				// Circlize
				V._circlize_hole << Rnd(-0.5,0.5),
				// Circus
				V._circus_size << Rnd(0.2,1.2),
				// Cloverleaf
				V._cloverleaf_filled << Rnd(0,1),
				// Collideoscope
				V.par1 << Rnd(1,10),
				V._collideoscope_kn_pi << V.par1 * M_1_PI,
				V._collideoscope_pi_kn << M_PI / V.par1,
				V.par2 << M_PI * Rnd01,
				V._collideoscope_ka_kn << V.par2 / V.par1,
				// Crop
				V._crop_xmin << Rnd(-0.8,0.0),
				V._crop_ymin << Rnd(-0.8,0.0),
				V._crop_xmax << V._crop_xmin + 0.9,
				V._crop_ymax << V._crop_ymin + 0.9,
				V.par1 << Rnd(-0.8,0.8),
				V._crop_w << (V._crop_xmax - V._crop_xmin) * 0.5 * V.par1,
				V._crop_h << (V._crop_ymax - V._crop_ymin) * 0.5 * V.par1,
				V._crop_zero << Rnd(0,1),
				// ECollide
				V.par1 << Rnd(1,10),
				V._ecollide_ecn_pi << V.par1 * M_1_PI,
				V._ecollide_pi_ecn << M_PI / V.par1,
				V.par2 << M_PI * Rnd(-1.01,0.99),
				V._ecollide_eca_ecn << V.par2 / V.par1,
			])
		]
}

def flCalc110_119 = { [
		
		Def("calc_110_119", 'calculate 110-119 %n %n %n',['func','X','Y'],
			[
				IfElse( EQ( Par.func, 110), // CannabisCurve
					[ V.par1 << (1.0 + 9.0 / 10.0 * Cos(8.0 * V.atan2xy)) * (1.0 + 1.0 / 10.0 * Cos(24.0 * V.atan2xy)) * (9.0 / 10.0 + 1.0 / 10.0 * Cos(200.0 * V.atan2xy)) * (1.0 + Sin(V.atan2xy)),
					  V.par2 << R2D*M_PI/2.0 + V.atan2xy,
					  If( EQ(V._cannabiscurve_filled,1.0), [V.par1 << V.par1 * Rnd01] ),
					  V.x << Sin(V.par2) * V.par1,
					  V.y << Cos(V.par2) * V.par1 + 1.0
					],
					[IfElse ( EQ( Par.func, 111), // Checks
						[ V.par1 << (Round(Par.X / V._checks_size) + Round(Par.Y / V._checks_size)) % 2, //isXY
						  V.par2 << V._checks_rnd * Rnd01,
						  V.par3 << V._checks_rnd * Rnd01,
						  IfElse( EQ(V.par1, 0.0),
							  [ V.par4 << -(V._checks_x) + V.par2,
								V.par5 << -(V._checks_y) 
							  ],
							  [ V.par4 << V._checks_x,
								V.par5 << V._checks_y + V.par3
							  ]
						  ),
						  V.x << Par.X + V.par4,
						  V.y << Par.Y + V.par5
						],
						[IfElse ( EQ( Par.func, 112), // CircleBlur
							[ V.par1 << Sqrt(Rnd01),
							  V.par2 << R2D * M_2PI * Rnd01,
							  V.x << Cos(V.par2) * V.par1,
							  V.y << Sin(V.par2) * V.par1
							],
							[IfElse ( EQ( Par.func, 113), // CircleCrop
								[ V.par1 << Par.X - V._circlecrop_x,
								  V.par2 << Par.Y - V._circlecrop_y,
								  V.par3 << Sqrt(V.par1 * V.par1 + V.par2 * V.par2), // rad
								  Call("atan2",[V.par2,V.par1,S.par4]), // ang
								  V.par5 << V._circlecrop_radius + Rnd01 * 0.5 * V._circlecrop_scatter_area, // rdc
								  IfElse( EQ(V._circlecrop_zero,1.0) & GT(V.par3,V._circlecrop_radius),
									  circlecropFlame() +
									  [ V.x << 0.0,
										V.y << 0.0
									  ],
								  	  [ IfElse( EQ(V._circlecrop_zero,1.0) & ~(GT(V.par3,V._circlecrop_radius)),
											[ V.x << Par.X,
											  V.y << Par.Y
											],
											[ IfElse( EQ(V._circlecrop_zero,0.0) & GT(V.par3,V._circlecrop_radius),
												[ V.x << V.par5 * Cos(V.par4) + V._circlecrop_x,
												  V.y << V.par5 * Sin(V.par4) + V._circlecrop_y
												],
												[ If( EQ(V._circlecrop_zero,0.0) & ~(GT(V.par3,V._circlecrop_radius)),
													[ V.x << Par.X,
													  V.y << Par.Y
													]
												  )
												]
											  )
											]
										)
									  ]
								  )
								],
								[IfElse ( EQ( Par.func, 114), // Circlize
									[ V.par1 << Abs(Par.X),
									  V.par2 << Abs(Par.Y),
									  IfElse( ~(LT( V.par1, V.par2)),
										  [ V.par3 << V.par1,
											IfElse( ~(LT(Par.X, V.par2)),
												[ V.par4 << V.par1 + Par.Y],
												[ V.par4 << 5.0 * V.par1 - Par.Y]
											)
										  ],
										  [ V.par3 << V.par2,
											IfElse( ~(LT(Par.Y, V.par1)),
												[ V.par4 << 3.0 * V.par2 - Par.X],
												[ V.par4 << 7.0 * V.par2 + Par.X]
											)
										  ]
									  ),
									  V.par1 << V.par3 / M_PI_4 + V._circlize_hole,
									  V.par2 << R2D * M_PI_4 * V.par4 / V.par3 - R2D * M_PI_4,
									  V.x << Cos(V.par2) * V.par1,
									  V.y << Sin(V.par2) * V.par1 
									],
									[IfElse ( EQ( Par.func, 115), // Circus
										[ IfElse( LT( V.r, 1.0),
											[ V.par1 << V.r * V._circus_size ],
											[ V.par1 << V.r / V._circus_size ]
										  ),
										  V.x << V.par1 * Cos(V.atan2yx),
										  V.y << V.par1 * Sin(V.atan2yx),
										],
										[IfElse ( EQ(Par.func, 116), // CloverLeaf
											[ V.par1 << (Sin(2.0 * V.atan2xy) + 0.25 * Sin(6.0 * V.atan2xy)),
											  If( EQ(V._cloverleaf_filled,1.0), [V.par1 << V.par1 * Rnd01] ),
											  V.x << Sin(V.atan2xy) * V.par1,
											  V.y << Cos(V.atan2xy) * V.par1
											],
											[IfElse ( EQ( Par.func, 117), // Collideoscope
												[ V.par1 << D2R * V.atan2yx,
												  IfElse( GT(V.par1, 0.0),
													  [ V.par2 << Floor(V.par1 * V._collideoscope_kn_pi),
														IfElse( EQ(V.par2 % 2.0, 0.0),
															[V.par1 << R2D * (V.par2 * V._collideoscope_pi_kn + (V._collideoscope_ka_kn + V.par1) % V._collideoscope_pi_kn)],
															[V.par1 << R2D * (V.par2 * V._collideoscope_pi_kn + (-(V._collideoscope_ka_kn) + V.par1) % V._collideoscope_pi_kn)]
														)
													  ],
													  [ V.par2 << Floor(-(V.par1) * V._collideoscope_kn_pi),
														IfElse( EQ(V.par2 % 2.0, 1.0),
															[V.par1 << -R2D * (V.par2 * V._collideoscope_pi_kn + (-(V._collideoscope_ka_kn) - V.par1) % V._collideoscope_pi_kn)],
															[V.par1 << -R2D * (V.par2 * V._collideoscope_pi_kn + (V._collideoscope_ka_kn - V.par1) % V._collideoscope_pi_kn)]
														)
													  ]
												  ),
												  V.x << V.r * Cos(V.par1),
												  V.y << V.r * Sin(V.par1)
												],
												[IfElse ( EQ( Par.func, 118), // Crop
													[ V.x << Par.X,
													  V.y << Par.Y,
													  IfElse( (LT(Par.X,V._crop_xmin) | GT(Par.X,V._crop_xmax) | LT(Par.Y,V._crop_ymin) | GT(Par.Y,V._crop_ymax)) & EQ(V._crop_zero,1.0),
														  [ V.x << 0.0,
															V.y << 0.0,
														  ],
														  [ IfElse( LT(Par.X, V._crop_xmin),
															  [ V.x << V._crop_xmin + Rnd01 * V._crop_w ],
															  [ If( GT(Par.X, V._crop_xmax), [ V.x << V._crop_xmax - Rnd01 * V._crop_w ]) ]
															),
															IfElse( LT(Par.Y, V._crop_ymin),
															  [ V.y << V._crop_ymin + Rnd01 * V._crop_h ],
															  [ If( GT(Par.Y, V._crop_ymax), [ V.y << V._crop_ymax - Rnd01 * V._crop_h ]) ]
														  )
														  ]
													  )
													],
													[ V.par1 << V.r2 + 1.0, // tmp  // ECollide
													  V.par2 << 2.0 * Par.X, // tmp2
													  V.par3 << V.par1 + V.par2,
													  IfElse( LT(V.par3,0),[V.par3 << 0.0],[V.par3 << Sqrt(V.par3)]),
													  V.par4 << V.par1 - V.par2,
													  IfElse( LT(V.par4,0),[V.par4 << 0.0],[V.par4 << Sqrt(V.par4)]),
													  V.par1 << (V.par3 + V.par4) * 0.5, // xmax
													  If( LT(V.par1,1.0), [V.par1 << 1.0]),
													  V.par2 << Par.X / V.par1,
													  IfElse( GT(V.par2,1.0), [V.par2 << 1.0], [If( LT(V.par2,-1.0),[V.par2<<-1.0])]),
													  V.par2 << D2R * ACos(V.par2), // nu
													  V.par3 << Round(V.par2 * V._ecollide_ecn_pi), // alt
													  IfElse( EQ(V.par3 % 2.0,0.0),
														  [V.par2 << V.par3 * V._ecollide_pi_ecn + (V.par2 + V._ecollide_eca_ecn) % V._ecollide_pi_ecn],
														  [V.par2 << V.par3 * V._ecollide_pi_ecn + (V.par2 - V._ecollide_eca_ecn) % V._ecollide_pi_ecn]
													  ),
													  If( LT(Par.Y,0.0),[ V.par2 << -(V.par2)]),
													  V.par2 << R2D * V.par2,
													  V.x << V.par1 * Cos(V.par2),
													  V.y << Sqrt(V.par1 - 1.0) * Sqrt(V.par1 + 1.0) * Sin(V.par2)
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
			)
		
]}

	def flPrecalc0_99 = { [
	
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
		V._fan2_x << Rnd(0.2,0.8),
		V._fan2_x << R2D * M_PI * V._fan2_x * V._fan2_x,
		V._fan2_y << R2D * Rnd(2,7),
		// Rings2
		V._rings2_val << Rnd(0.1,1.2),
		V._rings2_val << V._rings2_val * V._rings2_val,
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
		),
		// Supershape
		V._supershape_rnd << Rnd01,
		V._supershape_pm_4 << Rnd(1,7) / 4.0,
		V._supershape_pneg1_n1 << -1.0 / Rnd(0.1,10.0),
		V._supershape_n2 << Rnd(0.01,20.0),
		V._supershape_n3 << Rnd(0.01,20.0),
		V._supershape_holes << Rnd(-1.01,1.01),
		// Flower
		V._flower_petals << Rnd(3,8),
		V._flower_holes << Rnd(-0.5,0.2),
		// Conic
		V._conic_eccentricity << Rnd(0.3,1.0),
		V._conic_holes << Rnd01,
		// Parabola
		V._parabola_height << Rnd(0.5,1.5),
		V._parabola_width << Rnd(0.5,1.5),
		// Bent2
		V._bent2_x << Rnd(-1.5,1.5),
		V._bent2_y << Rnd(-1.5,1.5),
		// Bipolar
		V._bipolar_shift << -M_PI_2 * Rnd(-1.5,1.5),
		// Cell
		V._cell_size << Rnd(-2.5,2.5),
		// CPow
		V._cpow_power << Rnd(1,5),
		V._cpow_r << Rnd(1.01,3.0) / V._cpow_power,
		V._cpow_i << Rnd(-0.5,0.5) / V._cpow_power,
		V._cpow_va << 2.0 * M_PI / V._cpow_power,
		// Curve
		V._curve_xamp << Rnd(-2.5,2.5),
		V._curve_yamp << Rnd(-2.5,2.5),
		V._curve_xlength << Rnd(0.9,2.7),
		V._curve_xlength << V._curve_xlength * V._curve_xlength,
		V._curve_ylength << Rnd(0.9,2.7),
		V._curve_ylength << V._curve_ylength * V._curve_ylength,
		// Esher
		V.par1 << Rnd(-3.01,3.01) * R2D,
		V._esher_vd << 0.5 * Sin(V.par1),
		V._esher_vc << 0.5 * (1.0 + Cos(V.par1)),
		// Lazysuzan
		V._lazysusan_spin << R2D * Rnd(-3.01,3.01), // in degrees
		V._lazysusan_space << Rnd(-2.01,1.01), // in degrees
		V._lazysusan_twist << R2D * Rnd(-2.01,2.01), // in degrees
		V._lazysusan_x << Rnd(-0.5,0.5),
		V._lazysusan_y << Rnd(-0.5,0.5),
		// Modulus
		V._modulus_x << Rnd(-1.01,1.01),
		V._modulus_y << Rnd(-1.01,1.01),
		// Oscilloscope
		V._oscilloscope_separation << Rnd(0.01, 2.01),
		V._oscilloscope_frequency << 360.0 * Rnd(-3.01,3.01),
		V._oscilloscope_amplitude << Rnd(1.01,3.01),
		V._oscilloscope_damping << Rnd(0,1),
		// Popcorn2
		V._popcorn2_x << Rnd(-0.4,0.4),
		V._popcorn2_y << Rnd(-0.4,0.4),
		V._popcorn2_c << R2D * Rnd(-5.01,5.01),
		// Separation
		V._separation_x << Rnd01,
		V._separation_x << V._separation_x * V._separation_x,
		V._separation_xinside << Rnd(-1.01,1.01),
		V._separation_y << Rnd01,
		V._separation_y << V._separation_y * V._separation_y,
		V._separation_yinside << Rnd(-1.01,1.01),
		// Split
		V._split_xsize << Rnd(-0.8,0.8),
		V._split_ysize << Rnd(-0.8,0.8),
		// Splits
		V._splits_x << Rnd(-1.01,1.01),
		V._splits_y << Rnd(-1.01,1.01),
		// Stripes
		V._stripes_space << 1.0 - Rnd(-0.8,0.8),
		V._stripes_warp << Rnd(-4.01,4.01),
		// Wedge
		V._wedge_angle << Rnd(-3.01,3.01),
		V._wedge_hole << Rnd(-0.5,0.5),
		V._wedge_count << Rnd(1,6),
		V._wedge_swirl << Rnd(-1.01,1.01),
		// WegdeJulia
		V._wedgejulia_angle << Rnd(-3.01,3.01),
		V._wedgejulia_count << Rnd(2,7),
		V._wedgejulia_power << Rnd(2,7),
		V._wedgejulia_cf << 1.0 - 0.5 * M_1_PI * V._wedgejulia_angle * V._wedgejulia_count,
		V._wedgejulia_cn << Rnd(1.01,4.01) / V._wedgejulia_power / 2.0,
		// WedgeSph
		V._wedgesph_angle << Rnd(-3.01,3.01),
		V._wedgesph_count << Rnd(1,6),
		V._wedgesph_hole << Rnd(-0.5,0.5),
		V._wedgesph_swirl << Rnd(-1.01,1.01),
		// Whorl
		V._whorl_inside << Rnd(-1.01,1.01),
		V._whorl_outside << Rnd(-1.01,1.01),
		// Waves2
		V._waves2_freqx << Rnd(0.01,4.01),
		V._waves2_scalex << Rnd(0.5,1.5),
		V._waves2_freqy << Rnd(0.01,4.01),
		V._waves2_scaley << Rnd(0.5,1.5),
		// Auger
		V._auger_freq << R2D * Rnd(3,6),
		V._auger_scale << Rnd(0.1,0.8),
		V._auger_sym << Rnd(-1.01,1.01),
		V._auger_weight << Rnd(-1.01,1.01),
		// Flux
		V._flux_spread << 2.0 + Rnd(0.5,1.01),
		// Mobius
		V._mobius_re_a << Rnd(-1.01, 1.01),
		V._mobius_im_a << Rnd(-1.01, 1.01),
		V._mobius_re_b << Rnd(-1.01, 1.01),
		V._mobius_im_b << Rnd(-1.01, 1.01),
		V._mobius_re_c << Rnd(-1.01, 1.01),
		V._mobius_im_c << Rnd(-1.01, 1.01),
		V._mobius_re_d << Rnd(-1.01, 1.01),
		V._mobius_im_d << Rnd(-1.01, 1.01),
		// Truchet
		V._truchet_seed << Rnd(0,65535),
		V._truchet_seed2 << Sqrt(V._truchet_seed + V._truchet_seed / 2.0 + 0.000000001) / (V._truchet_seed * 0.5 + 0.000000001) * 0.25,
		V._truchet_exponent << Rnd(0.5,2.0), // exponent
		V._truchet_onen << 1.0 / V._truchet_exponent,
		Call("pow",[2.0, 1.0/ V._truchet_exponent, S.par1]),
		V._truchet_rmax << 0.5 * Rnd(0.2,0.8) * (V.par1 - 1.0),
		V._truchet_size << Rnd(0.8,3.0),
		V.par1 << Rnd(-360,0),
		V._truchet_scale << Cos(V.par1) - Sin(V.par1)
	])
] }

	def flPrecalc100_109 = {
		[
			Script([ 
				// BCollide
				V.par1 << Rnd(1.1,8.1),
				V._bcollide_bcn_pi << M_1_PI * V.par1,
				V._bcollide_pi_bcn << M_PI / V.par1,
				V._bcollide_bca_bcn << (M_PI * Rnd(-0.5,1)) / V.par1,
				// BMod
				V._bmod_radius << Rnd(0.01,M_2PI),
				V._bmod_distance << Rnd(0.01,2.0),
				// BSwirl
				V._bswirl_in << Rnd(-2.01,2.01),
				V._bswirl_out << Rnd(-2.01,2.01),
				// BTransform
				V._btransform_rotate << Rnd(-M_2PI,M_2PI),
				V._btransform_power << Rnd(1.01,6.0),
				V._btransform_move << Rnd(-0.5,0.5),
				V._btransform_split << Rnd(-1.01,1.01),
				// BWraps7
				V._bwraps7_cellsize << Rnd(0.4,1.5),
				V.par1 << Rnd(0.5,2.5), // gain
				V._bwraps7_g2 << V.par1 * V.par1 + 1.0e-6,
				V._bwraps7_inner_twist << Rnd(-M_PI,M_PI),
				V._bwraps7_outer_twist << Rnd(-M_PI,M_PI),
				V.par1 << Rnd(0,0.7), // space
				V.par1 << 0.5 * (V._bwraps7_cellsize / (1.0 + V.par1 * V.par1)), // radius
				V.par2 << V._bwraps7_g2 * V.par1,
				IfElse( GT(V.par2,2.0),
					[V.par2 << 1.0],
					[V.par2 << V.par2 * (1.0 / ( (V.par2 * V.par2) / 4.0 + 1.0 ))]
				),
				V._bwraps7_r2 << V.par1 * V.par1,
				V._bwraps7_rfactor << V.par1 / V.par2,
				// Barycentroid
				V._barycentroid_a << Rnd(-1.01,1.01),
				V._barycentroid_b << Rnd(-1.01,1.01),
				V._barycentroid_c << Rnd(-1.01,1.01),
				V._barycentroid_d << Rnd(-1.01,1.01),
				V._barycentroid_dot00 << V._barycentroid_a * V._barycentroid_a + V._barycentroid_b + V._barycentroid_b,
				V._barycentroid_dot01 << V._barycentroid_a * V._barycentroid_c + V._barycentroid_b * V._barycentroid_d,
				V._barycentroid_dot11 << V._barycentroid_c * V._barycentroid_c + V._barycentroid_d * V._barycentroid_d,
				V._barycentroid_invdenom << 1.0 / (V._barycentroid_dot00 * V._barycentroid_dot11 - V._barycentroid_dot01 * V._barycentroid_dot01),
				// BlockY
				V._blocky_x << (1.0 - 2.0 * Rnd(0,1)) * Rnd(0.9,1.01),
				V._blocky_y << (1.0 - 2.0 * Rnd(0,1)) * Rnd(0.9,1.01),
				V._blocky_mp << (1.0 - 2.0 * Rnd(0,1)) * Rnd(1.5,6.01),
				// BlurZoom
				V._blurzoom_length << Rnd(-1.01,1.5),
				V._blurzoom_x << Rnd(-1.01,1.01),
				V._blurzoom_y << Rnd(-1.01,1.01),
				// Boarders2
				V._boarders2_c << Rnd01 + 0.1,
				V._boarders2_cl << V._boarders2_c * (Rnd01 + 0.1),
				V._boarders2_cr << V._boarders2_c + V._boarders2_c * (Rnd01 + 0.1)
			])
	    ]
	}
	
}

def gen = new FFVars();


gen.printScripts (
	gen.flFunctions() + 
	gen.flCalc110_119() +
	gen.flPrecalc110_119()
)

gen.printVariables()
