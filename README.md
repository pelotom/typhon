# Typhon
---

Typhon is a toy language that compiles to Java. The type inferencer is mostly a simplified version of the algorithm presented in [this paper](http://research.microsoft.com/en-us/um/people/simonpj/papers/higher-rank/putting.pdf) (it does not currently support arbitrary-rank types!) The pattern matching algorithm comes from chapter 5 of [this book](http://research.microsoft.com/en-us/um/people/simonpj/papers/slpj-book-1987/).

## Example
Here's the Java code that gets generated for the definition of a list:
```java
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
```
Here's the generated code for the [humble `zipWith` function](http://hackage.haskell.org/packages/archive/base/latest/doc/html/src/GHC-List.html#zipWith):
```java
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
```