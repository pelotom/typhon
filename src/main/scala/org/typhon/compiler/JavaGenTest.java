package org.typhon.compiler;

public class JavaGenTest {

	public static interface $F<a, b> {
		b $apply(a a);
	}

	public static abstract class P1<a> {
		private static final class $P1<a> extends P1<a> {
			private final a $1;

			private $P1(final a $1) {
				this.$1 = $1;
			}

			public <M> M match(final $F<a, M> $ifP1) {
				return $ifP1.$apply($1);
			}
		}

		public abstract <M> M match(final $F<a, M> $ifP1);
	}

	public static abstract class P2<a, b> {
		private static final class $P2<a, b> extends P2<a, b> {
			private final a $1;
			private final b $2;

			private $P2(final a $1, final b $2) {
				this.$1 = $1;
				this.$2 = $2;
			}

			public <M> M match(final $F<a, $F<b, M>> $ifP2) {
				return $ifP2.$apply($1).$apply($2);
			}
		}

		public abstract <M> M match(final $F<a, $F<b, M>> $ifP2);
	}

	public static abstract class P3<a, b, c> {
		private static final class $P3<a, b, c> extends P3<a, b, c> {
			private final a $1;
			private final b $2;
			private final c $3;

			private $P3(final a $1, final b $2, final c $3) {
				this.$1 = $1;
				this.$2 = $2;
				this.$3 = $3;
			}

			public <M> M match(final $F<a, $F<b, $F<c, M>>> $ifP3) {
				return $ifP3.$apply($1).$apply($2).$apply($3);
			}
		}

		public abstract <M> M match(final $F<a, $F<b, $F<c, M>>> $ifP3);
	}

	public static abstract class List<a> {
		private static final class $Nil<a> extends List<a> {
			private $Nil() {
			}

			public <M> M match(final M $ifNil, final $F<a, $F<List<a>, M>> $ifCons) {
				return $ifNil;
			}
		}

		private static final class $Cons<a> extends List<a> {
			private final a $1;
			private final List<a> $2;

			private $Cons(final a $1, final List<a> $2) {
				this.$1 = $1;
				this.$2 = $2;
			}

			public <M> M match(final M $ifNil, final $F<a, $F<List<a>, M>> $ifCons) {
				return $ifCons.$apply($1).$apply($2);
			}
		}

		public abstract <M> M match(final M $ifNil, final $F<a, $F<List<a>, M>> $ifCons);
	}

	public static final <a> $F<a, P1<a>> P1(final a $a) {
		return new $F<a, P1<a>>() {
			public P1<a> $apply(final a $1) {
				return new P1.$P1<a>($1);
			}
		};
	}

	public static final <a, b> $F<a, $F<b, P2<a, b>>> P2(final a $a, final b $b) {
		return new $F<a, $F<b, P2<a, b>>>() {
			public $F<b, P2<a, b>> $apply(final a $1) {
				return new $F<b, P2<a, b>>() {
					public P2<a, b> $apply(final b $2) {
						return new P2.$P2<a, b>($1, $2);
					}
				};
			}
		};
	}

	public static final <a, b, c> $F<a, $F<b, $F<c, P3<a, b, c>>>> P3(final a $a, final b $b, final c $c) {
		return new $F<a, $F<b, $F<c, P3<a, b, c>>>>() {
			public $F<b, $F<c, P3<a, b, c>>> $apply(final a $1) {
				return new $F<b, $F<c, P3<a, b, c>>>() {
					public $F<c, P3<a, b, c>> $apply(final b $2) {
						return new $F<c, P3<a, b, c>>() {
							public P3<a, b, c> $apply(final c $3) {
								return new P3.$P3<a, b, c>($1, $2, $3);
							}
						};
					}
				};
			}
		};
	}

	public static final <a> List<a> Nil(final a $a) {
		return new List.$Nil<a>();
	}

	public static final <a> $F<a, $F<List<a>, List<a>>> Cons(final a $a) {
		return new $F<a, $F<List<a>, List<a>>>() {
			public $F<List<a>, List<a>> $apply(final a $1) {
				return new $F<List<a>, List<a>>() {
					public List<a> $apply(final List<a> $2) {
						return new List.$Cons<a>($1, $2);
					}
				};
			}
		};
	}

	public static final <a, b> $F<$F<a, b>, $F<List<a>, List<b>>> map(final a $a, final b $b) {
		return new $F<$F<a, b>, $F<List<a>, List<b>>>() {
			public $F<List<a>, List<b>> $apply(final $F<a, b> f) {
				return new $F<List<a>, List<b>>() {
					public List<b> $apply(final List<a> xs) {
						return new Object() {
							private final List<a> $m7 = xs;

							private List<b> body() {
								return ($m7).match(Nil((b) null), new $F<a, $F<List<a>, List<b>>>() {
									public $F<List<a>, List<b>> $apply(final a $m15) {
										return new $F<List<a>, List<b>>() {
											public List<b> $apply(final List<a> $m16) {
												return Cons((b) null).$apply(f.$apply($m15)).$apply(map((a) null, (b) null).$apply(f).$apply($m16));
											}
										};
									}
								});
							}
						}.body();
					}
				};
			}
		};
	}

	public static final <a, b, c> $F<$F<a, $F<b, c>>, $F<List<a>, $F<List<b>, List<c>>>> zipWith(final a $a, final b $b, final c $c) {
		return new $F<$F<a, $F<b, c>>, $F<List<a>, $F<List<b>, List<c>>>>() {
			public $F<List<a>, $F<List<b>, List<c>>> $apply(final $F<a, $F<b, c>> f) {
				return new $F<List<a>, $F<List<b>, List<c>>>() {
					public $F<List<b>, List<c>> $apply(final List<a> xs) {
						return new $F<List<b>, List<c>>() {
							public List<c> $apply(final List<b> ys) {
								return new Object() {
									private final P3<$F<a, $F<b, c>>, List<a>, List<b>> $m53 = P3(($F<a, $F<b, c>>) null, (List<a>) null, (List<b>) null).$apply(f).$apply(xs).$apply(ys);

									private List<c> body() {
										return ($m53).match(new $F<$F<a, $F<b, c>>, $F<List<a>, $F<List<b>, List<c>>>>() {
											public $F<List<a>, $F<List<b>, List<c>>> $apply(final $F<a, $F<b, c>> $m61) {
												return new $F<List<a>, $F<List<b>, List<c>>>() {
													public $F<List<b>, List<c>> $apply(final List<a> $m62) {
														return new $F<List<b>, List<c>>() {
															public List<c> $apply(final List<b> $m63) {
																return ($m62).match(Nil((c) null), new $F<a, $F<List<a>, List<c>>>() {
																	public $F<List<a>, List<c>> $apply(final a $m67) {
																		return new $F<List<a>, List<c>>() {
																			public List<c> $apply(final List<a> $m68) {
																				return ($m63).match(Nil((c) null), new $F<b, $F<List<b>, List<c>>>() {
																					public $F<List<b>, List<c>> $apply(final b $m72) {
																						return new $F<List<b>, List<c>>() {
																							public List<c> $apply(final List<b> $m73) {
																								return Cons((c) null).$apply($m61.$apply($m67).$apply($m72)).$apply(zipWith((a) null, (b) null, (c) null).$apply($m61).$apply($m68).$apply($m73));
																							}
																						};
																					}
																				});
																			}
																		};
																	}
																});
															}
														};
													}
												};
											}
										});
									}
								}.body();
							}
						};
					}
				};
			}
		};
	}

	private static <a> $F<String, a> error(final a _a) {
		return new $F<String, a>() {
			public a $apply(final String msg) {
				throw new Error(msg);
			}
		};
	}

	public static void main(String[] args) {
		Object o;
		
		// Failed to type (\x -> (x x)). Occurs check: cannot construct the infinite
		// type: a = (a -> b)

		// True :: $0
		// ->True
		o = true;
		System.err.println("True\n" + o);

		// (\x -> True) :: $0
		// ->(\(x::$1) -> (True::(Object)))
		o = new $F<Object, Object>() {
			public Object $apply(final Object x) {
				return true;
			}
		};
		System.err.println("(\\x -> True)\n" + o);

		// (\x f -> (f (x 5))) :: $0
		// ->(\(x::((Int) -> $6)) (f::($6 -> $4)) -> ((f (x 5))::$4))
		o = new $F<$F<Integer, Object>, $F<$F<Object, Object>, Object>>() {
			public $F<$F<Object, Object>, Object> $apply(final $F<Integer, Object> x) {
				return new $F<$F<Object, Object>, Object>() {
					public Object $apply(final $F<Object, Object> f) {
						return f.$apply(x.$apply(5));
					}
				};
			}
		};
		System.err.println("(\\x f -> (f (x 5)))\n" + o);

		// Failed to type (5 hello). Cannot unify '(Int)' with '(a -> b)'

		// (let id = (\x -> x) in (id id)) :: $0
		// ->(let (id::(forall a . (a -> a))) = (\(x::a) -> (x::a)) in ((id[($8 ->
		// $8)] id[$8])::($8 -> $8))
		o = new Object() {
			private final <a> $F<a, a> id(final a $a) {
				return new $F<a, a>() {
					public a $apply(final a x) {
						return x;
					}
				};
			}

			private $F<Object, Object> body() {
				return id(($F<Object, Object>) null).$apply(id((Object) null));
			}
		}.body();
		System.err.println("(let id = (\\x -> x) in (id id))\n" + o);

		// (let fix = (\x -> (fix (fix x))) in fix) :: $0
		// ->(let (fix::(forall a . (a -> a))) = (\(x::a) -> ((fix[a] (fix[a]
		// x))::a)) in (fix[$10]::($10 -> $10))
		o = new Object() {
			private final <a> $F<a, a> fix(final a $a) {
				return new $F<a, a>() {
					public a $apply(final a x) {
						return fix((a) null).$apply(fix((a) null).$apply(x));
					}
				};
			}

			private $F<Object, Object> body() {
				return fix((Object) null);
			}
		}.body();
		System.err.println("(let fix = (\\x -> (fix (fix x))) in fix)\n" + o);

		// (let id = (\x -> x) in ((P2 (id True)) (id 9))) :: $0
		// ->(let (id::(forall a . (a -> a))) = (\(x::a) -> (x::a)) in
		// (((P2[(Object),(Int)] (id[(Object)] True)) (id[(Int)] 9))::(P2 (Object)
		// (Int)))
		o = new Object() {
			private final <a> $F<a, a> id(final a $a) {
				return new $F<a, a>() {
					public a $apply(final a x) {
						return x;
					}
				};
			}

			private P2<Object, Integer> body() {
				return P2((Object) null, (Integer) null).$apply(id((Object) null).$apply(true)).$apply(id((Integer) null).$apply(9));
			}
		}.body();
		System.err.println("(let id = (\\x -> x) in ((P2 (id True)) (id 9)))\n" + o);

		// (let bar = (\x -> (let foo = (\y -> x) in foo)) in bar) :: $0
		// ->(let (bar::(forall b c . (b -> (c -> b)))) = (\(x::b) -> ((let
		// (foo::(forall a . (a -> b))) = (\(y::a) -> (x::b)) in (foo[c]::(c ->
		// b))::b)) in (bar[$8,$9]::($8 -> ($9 -> $8)))
		o = new Object() {
			private final <b, c> $F<b, $F<c, b>> bar(final b $b, final c $c) {
				return new $F<b, $F<c, b>>() {
					public $F<c, b> $apply(final b x) {
						return new Object() {
							private final <a> $F<a, b> foo(final a $a) {
								return new $F<a, b>() {
									public b $apply(final a y) {
										return x;
									}
								};
							}

							private $F<c, b> body() {
								return foo((c) null);
							}
						}.body();
					}
				};
			}

			private $F<Object, $F<Object, Object>> body() {
				return bar((Object) null, (Object) null);
			}
		}.body();
		System.err.println("(let bar = (\\x -> (let foo = (\\y -> x) in foo)) in bar)\n" + o);

		// Failed to type (f (\x -> True)). Cannot unify '(Int)' with '(Object)'

		// ((\f -> (f ((P2 32) True))) (\x -> x)) :: $0
		// ->((\(f::((P2 (Int) (Object)) -> (P2 (Int) (Object)))) -> ((f
		// ((P2[(Int),(Object)] 32) True))::(P2 (Int) (Object)))) (\(x::(P2 (Int)
		// (Object))) -> (x::(P2 (Int) (Object)))))
		o = new $F<$F<P2<Integer, Object>, P2<Integer, Object>>, P2<Integer, Object>>() {
			public P2<Integer, Object> $apply(final $F<P2<Integer, Object>, P2<Integer, Object>> f) {
				return f.$apply(P2((Integer) null, (Object) null).$apply(32).$apply(true));
			}
		}.$apply(new $F<P2<Integer, Object>, P2<Integer, Object>>() {
			public P2<Integer, Object> $apply(final P2<Integer, Object> x) {
				return x;
			}
		});
		System.err.println("((\\f -> (f ((P2 32) True))) (\\x -> x))\n" + o);

		// (\f -> ((P2 (f P2)) (f P2))) :: $0
		// ->(\(f::(($19 -> ($20 -> (P2 $19 $20))) -> $14)) -> (((P2[$14,$14] (f
		// P2[$19,$20])) (f P2[$19,$20]))::(P2 $14 $14)))
		o = new $F<$F<$F<Object, $F<Object, P2<Object, Object>>>, Object>, P2<Object, Object>>() {
			public P2<Object, Object> $apply(final $F<$F<Object, $F<Object, P2<Object, Object>>>, Object> f) {
				return P2((Object) null, (Object) null).$apply(f.$apply(P2((Object) null, (Object) null))).$apply(f.$apply(P2((Object) null, (Object) null)));
			}
		};
		System.err.println("(\\f -> ((P2 (f P2)) (f P2)))\n" + o);

		// Failed to type (let f = (f 9) in f). Occurs check: cannot construct the
		// infinite type: a = ((Int) -> a)

		// (\x y -> ((x y) 9)) :: $0
		// ->(\(x::($7 -> ((Int) -> $4))) (y::$7) -> (((x y) 9)::$4))
		o = new $F<$F<Object, $F<Integer, Object>>, $F<Object, Object>>() {
			public $F<Object, Object> $apply(final $F<Object, $F<Integer, Object>> x) {
				return new $F<Object, Object>() {
					public Object $apply(final Object y) {
						return x.$apply(y).$apply(9);
					}
				};
			}
		};
		System.err.println("(\\x y -> ((x y) 9))\n" + o);

		// (let f = (\x -> x); y = (\x -> (f x)) in y) :: $0
		// ->(let (f::(forall a . (a -> a))) = (\(x::a) -> (x::a)); (y::(forall a .
		// (a -> a))) = (\(x::a) -> ((f[a] x)::a)) in (y[$11]::($11 -> $11))
		o = new Object() {
			private final <a> $F<a, a> f(final a $a) {
				return new $F<a, a>() {
					public a $apply(final a x) {
						return x;
					}
				};
			}

			private final <a> $F<a, a> y(final a $a) {
				return new $F<a, a>() {
					public a $apply(final a x) {
						return f((a) null).$apply(x);
					}
				};
			}

			private $F<Object, Object> body() {
				return y((Object) null);
			}
		}.body();
		System.err.println("(let f = (\\x -> x); y = (\\x -> (f x)) in y)\n" + o);

		// (let f = (\x -> x); y = (\x -> (f x)) in (y 9)) :: $0
		// ->(let (f::(forall a . (a -> a))) = (\(x::a) -> (x::a)); (y::(forall a .
		// (a -> a))) = (\(x::a) -> ((f[a] x)::a)) in ((y[(Int)] 9)::(Int))
		o = new Object() {
			private final <a> $F<a, a> f(final a $a) {
				return new $F<a, a>() {
					public a $apply(final a x) {
						return x;
					}
				};
			}

			private final <a> $F<a, a> y(final a $a) {
				return new $F<a, a>() {
					public a $apply(final a x) {
						return f((a) null).$apply(x);
					}
				};
			}

			private Integer body() {
				return y((Integer) null).$apply(9);
			}
		}.body();
		System.err.println("(let f = (\\x -> x); y = (\\x -> (f x)) in (y 9))\n" + o);

		// (let f = (\x -> x); y = (\x -> (f x)) in ((P2 (y 9)) (y True))) :: $0
		// ->(let (f::(forall a . (a -> a))) = (\(x::a) -> (x::a)); (y::(forall a .
		// (a -> a))) = (\(x::a) -> ((f[a] x)::a)) in (((P2[(Int),(Object)]
		// (y[(Int)] 9)) (y[(Object)] True))::(P2 (Int) (Object)))
		o = new Object() {
			private final <a> $F<a, a> f(final a $a) {
				return new $F<a, a>() {
					public a $apply(final a x) {
						return x;
					}
				};
			}

			private final <a> $F<a, a> y(final a $a) {
				return new $F<a, a>() {
					public a $apply(final a x) {
						return f((a) null).$apply(x);
					}
				};
			}

			private P2<Integer, Object> body() {
				return P2((Integer) null, (Object) null).$apply(y((Integer) null).$apply(9)).$apply(y((Object) null).$apply(true));
			}
		}.body();
		System.err.println("(let f = (\\x -> x); y = (\\x -> (f x)) in ((P2 (y 9)) (y True)))\n" + o);

		// (let f = (\x -> x); y = ((P2 (f 6)) (f True)) in y) :: $0
		// ->(let (f::(forall a . (a -> a))) = (\(x::a) -> (x::a)); (y::((P2 (Int)
		// (Object)))) = ((P2[(Int),(Object)] (f[(Int)] 6)) (f[(Object)] True)) in
		// (y::(P2 (Int) (Object)))
		o = new Object() {
			private final <a> $F<a, a> f(final a $a) {
				return new $F<a, a>() {
					public a $apply(final a x) {
						return x;
					}
				};
			}

			private final P2<Integer, Object> y = P2((Integer) null, (Object) null).$apply(f((Integer) null).$apply(6)).$apply(f((Object) null).$apply(true));

			private P2<Integer, Object> body() {
				return y;
			}
		}.body();
		System.err.println("(let f = (\\x -> x); y = ((P2 (f 6)) (f True)) in y)\n" + o);

		// Failed to type (let f = (\x -> (let a = y in x)); y = ((P2 (f 6)) (f
		// True)) in y). Cannot unify '(Int)' with '(Object)'

		// (case ((P2 True) 9) of (P2 a b) -> a) :: $0
		// ->(let ($m10::((P2 (Object) (Int)))) = ((P2[(Object),(Int)] True) 9) in
		// ((case $m10 of (\($m17::(Object)) ($m18::(Int)) ->
		// ($m17::(Object))))::(Object))
		o = new Object() {
			private final P2<Object, Integer> $m10 = P2((Object) null, (Integer) null).$apply(true).$apply(9);

			private Object body() {
				return ($m10).match(new $F<Object, $F<Integer, Object>>() {
					public $F<Integer, Object> $apply(final Object $m17) {
						return new $F<Integer, Object>() {
							public Object $apply(final Integer $m18) {
								return $m17;
							}
						};
					}
				});
			}
		}.body();
		System.err.println("(case ((P2 True) 9) of (P2 a b) -> a)\n" + o);

		// (case ((P2 True) 9) of (P2 a b) -> a; (P2 x y) -> y) :: $0
		// ->(let ($m10::((P2 (Object) (Int)))) = ((P2[(Object),(Int)] True) 9) in
		// ((case $m10 of (\($m17::(Object)) ($m18::(Int)) ->
		// ($m17::(Object))))::(Object))
		o = new Object() {
			private final P2<Object, Integer> $m10 = P2((Object) null, (Integer) null).$apply(true).$apply(9);

			private Object body() {
				return ($m10).match(new $F<Object, $F<Integer, Object>>() {
					public $F<Integer, Object> $apply(final Object $m17) {
						return new $F<Integer, Object>() {
							public Object $apply(final Integer $m18) {
								return $m17;
							}
						};
					}
				});
			}
		}.body();
		System.err.println("(case ((P2 True) 9) of (P2 a b) -> a; (P2 x y) -> y)\n" + o);

	}
}
