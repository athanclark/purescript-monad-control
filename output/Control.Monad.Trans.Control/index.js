"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad_Base = require("../Control.Monad.Base/index.js");
var Control_Monad_Except_Trans = require("../Control.Monad.Except.Trans/index.js");
var Control_Monad_Free_Trans = require("../Control.Monad.Free.Trans/index.js");
var Control_Monad_Maybe_Trans = require("../Control.Monad.Maybe.Trans/index.js");
var Control_Monad_RWS_Trans = require("../Control.Monad.RWS.Trans/index.js");
var Control_Monad_Reader_Trans = require("../Control.Monad.Reader.Trans/index.js");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class/index.js");
var Control_Monad_State_Trans = require("../Control.Monad.State.Trans/index.js");
var Control_Monad_Trans_Class = require("../Control.Monad.Trans.Class/index.js");
var Control_Monad_Writer_Trans = require("../Control.Monad.Writer.Trans/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Functor_Compose = require("../Data.Functor.Compose/index.js");
var Data_Identity = require("../Data.Identity/index.js");
var Data_List = require("../Data.List/index.js");
var Data_List_Types = require("../Data.List.Types/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Effect = require("../Effect/index.js");
var Effect_Aff = require("../Effect.Aff/index.js");
var Prelude = require("../Prelude/index.js");
var WriterTStT = (function () {
    function WriterTStT(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    WriterTStT.create = function (value0) {
        return function (value1) {
            return new WriterTStT(value0, value1);
        };
    };
    return WriterTStT;
})();

// liftEffectWith :: ((Aff eff a -> Eff eff a) -> Eff eff a) -> Aff eff a
// liftEffectWith f = makeAff \onError onSuccess ->
//   f (\a -> runAff onError onSuccess a)
var FreeTStT = function (x) {
    return x;
};
var MonadTransControl = function (Monad1, MonadTrans0, liftWith, restoreT) {
    this.Monad1 = Monad1;
    this.MonadTrans0 = MonadTrans0;
    this.liftWith = liftWith;
    this.restoreT = restoreT;
};
var MonadBaseControl = function (MonadBase0, liftBaseWith, restoreM) {
    this.MonadBase0 = MonadBase0;
    this.liftBaseWith = liftBaseWith;
    this.restoreM = restoreM;
};
var writerTStTToTuple = function (v) {
    return new Data_Tuple.Tuple(v.value1, v.value0);
};
var tupleToWriterTStT = function (v) {
    return new WriterTStT(v.value1, v.value0);
};
var writerTMonadTransControl = function (dictMonoid) {
    return function (dictMonad) {
        return new MonadTransControl(function () {
            return dictMonad;
        }, function () {
            return Control_Monad_Writer_Trans.monadTransWriterT(dictMonoid);
        }, function (f) {
            return Control_Monad_Trans_Class.lift(Control_Monad_Writer_Trans.monadTransWriterT(dictMonoid))(dictMonad)(f(function (x) {
                return Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0())(tupleToWriterTStT)(Control_Monad_Writer_Trans.runWriterT(x));
            }));
        }, function (x) {
            return Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0())(writerTStTToTuple)(x);
        });
    };
};
var stateTMonadTransControl = function (dictMonad) {
    return new MonadTransControl(function () {
        return dictMonad;
    }, function () {
        return Control_Monad_State_Trans.monadTransStateT;
    }, function (f) {
        return function (s) {
            return Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0())(function (b) {
                return new Data_Tuple.Tuple(b, s);
            })(f(function (x) {
                return Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0())(tupleToWriterTStT)(Control_Monad_State_Trans.runStateT(x)(s));
            }));
        };
    }, function (x) {
        return function (v) {
            return Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0())(writerTStTToTuple)(x);
        };
    });
};
var rwsTMonadTransControl = function (dictMonoid) {
    return function (dictMonad) {
        return new MonadTransControl(function () {
            return dictMonad;
        }, function () {
            return Control_Monad_RWS_Trans.monadTransRWST(dictMonoid);
        }, function (f) {
            return function (r) {
                return function (s) {
                    return Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0())(function (x) {
                        return new Control_Monad_RWS_Trans.RWSResult(s, x, Data_Monoid.mempty(dictMonoid));
                    })(f(function (t) {
                        return Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0())(function (v) {
                            return new Data_Tuple.Tuple(v.value2, new Data_Tuple.Tuple(v.value0, v.value1));
                        })(Control_Monad_RWS_Trans.runRWST(t)(r)(s));
                    }));
                };
            };
        }, function (mSt) {
            return function (v) {
                return function (v1) {
                    return Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0())(function (v2) {
                        return new Control_Monad_RWS_Trans.RWSResult(v2.value1.value0, v2.value1.value1, v2.value0);
                    })(mSt);
                };
            };
        });
    };
};
var runIdentity = function (v) {
    return v;
};
var tupleMonadBaseControl = function (dictMonoid) {
    return new MonadBaseControl(function () {
        return Control_Monad_Base.tupleMonadBase(dictMonoid);
    }, function (f) {
        return f(Data_Functor.map(Data_Tuple.functorTuple)(Data_Identity.Identity));
    }, Data_Functor.map(Data_Tuple.functorTuple)(runIdentity));
};
var runCompose = function (v) {
    return v;
};
var restoreT = function (dict) {
    return dict.restoreT;
};
var restoreM = function (dict) {
    return dict.restoreM;
};
var readerTMonadTransControl = function (dictMonad) {
    return new MonadTransControl(function () {
        return dictMonad;
    }, function () {
        return Control_Monad_Reader_Trans.monadTransReaderT;
    }, function (f) {
        return function (r) {
            return f(function (v) {
                return Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0())(Data_Identity.Identity)(v(r));
            });
        };
    }, function (x) {
        return Data_Functor.map(Control_Monad_Reader_Trans.functorReaderT(((dictMonad.Bind1()).Apply0()).Functor0()))(runIdentity)(Control_Monad_Trans_Class.lift(Control_Monad_Reader_Trans.monadTransReaderT)(dictMonad)(x));
    });
};

// FIXME 0.11.0's -transformers library actually exports the goods >.>
// instance listTMonadTransControl :: MonadTransControl ListT List where
//   liftWith f = ListT $ (\x -> Cons x Nil) <$> f (\(ListT x) -> x)
//   restoreT = ListT
var maybeTMonadTransControl = function (dictMonad) {
    return new MonadTransControl(function () {
        return dictMonad;
    }, function () {
        return Control_Monad_Maybe_Trans.monadTransMaybeT;
    }, function (f) {
        return Control_Monad_Maybe_Trans.MaybeT(Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0())(Data_Maybe.Just.create)(f(Control_Monad_Maybe_Trans.runMaybeT)));
    }, Control_Monad_Maybe_Trans.MaybeT);
};
var maybeMonadBaseControl = new MonadBaseControl(function () {
    return Control_Monad_Base.maybeMonadBase;
}, function (f) {
    return f(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Identity.Identity));
}, Data_Functor.map(Data_Maybe.functorMaybe)(runIdentity));
var listMonadBaseControl = new MonadBaseControl(function () {
    return Control_Monad_Base.listMonadBase;
}, function (f) {
    return f(Data_Functor.map(Data_List_Types.functorList)(Data_Identity.Identity));
}, Data_Functor.map(Data_List_Types.functorList)(runIdentity));
var liftWith = function (dict) {
    return dict.liftWith;
};
var liftBaseWith = function (dict) {
    return dict.liftBaseWith;
};

// | Pack a state belonging to `t` back into it, instead of throwing it away
var integrateT = function (dictBind) {
    return function (dictMonadTransControl) {
        return function (x) {
            return Control_Bind.join(dictBind)(Data_Functor.map((dictBind.Apply0()).Functor0())(function ($94) {
                return restoreT(dictMonadTransControl)(Control_Applicative.pure((dictMonadTransControl.Monad1()).Applicative0())($94));
            })(x));
        };
    };
};

// | Pack a state belonging to `m` back into it, instead of throwing it away
var integrateM = function (dictApplicative) {
    return function (dictMonadBaseControl) {
        return function (x) {
            return Control_Bind.join(((dictMonadBaseControl.MonadBase0()).Monad1()).Bind1())(Data_Functor.map(((((dictMonadBaseControl.MonadBase0()).Monad1()).Bind1()).Apply0()).Functor0())(function ($95) {
                return restoreM(dictMonadBaseControl)(Control_Applicative.pure(dictApplicative)($95));
            })(x));
        };
    };
};
var identityMonadBaseControl = new MonadBaseControl(function () {
    return Control_Monad_Base.identityMonadBase;
}, function (f) {
    return f(Data_Functor.map(Data_Identity.functorIdentity)(Data_Identity.Identity));
}, Data_Functor.map(Data_Identity.functorIdentity)(runIdentity));
var functorWriterTStT = new Data_Functor.Functor(function (f) {
    return function (v) {
        return new WriterTStT(v.value0, f(v.value1));
    };
});
var functorFreeTStT = function (dictFunctor) {
    return function (dictMonad) {
        return new Data_Functor.Functor(function (f) {
            return function (v) {
                return FreeTStT((function () {
                    if (v instanceof Data_Either.Left) {
                        return new Data_Either.Left(f(v.value0));
                    };
                    if (v instanceof Data_Either.Right) {
                        return new Data_Either.Right(Data_Functor.map(dictFunctor)(Data_Functor.map(Control_Monad_Free_Trans.functorFreeT(dictFunctor)(((dictMonad.Bind1()).Apply0()).Functor0()))(f))(v.value0));
                    };
                    throw new Error("Failed pattern match at Control.Monad.Trans.Control line 76, column 35 - line 78, column 37: " + [ v.constructor.name ]);
                })());
            };
        });
    };
};
var funcMonadBaseControl = new MonadBaseControl(function () {
    return Control_Monad_Base.functionMonadBase;
}, function (f) {
    return f(Data_Functor.map(Data_Functor.functorFn)(Data_Identity.Identity));
}, Data_Functor.map(Data_Functor.functorFn)(runIdentity));
var freeTStTToEither = function (v) {
    return v;
};
var exceptTMonadTransControl = function (dictMonad) {
    return new MonadTransControl(function () {
        return dictMonad;
    }, function () {
        return Control_Monad_Except_Trans.monadTransExceptT;
    }, function (f) {
        return Control_Monad_Except_Trans.ExceptT(Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0())(Data_Either.Right.create)(f(Control_Monad_Except_Trans.runExceptT)));
    }, Control_Monad_Except_Trans.ExceptT);
};
var eitherToFreeTStT = function (x) {
    return x;
};
var freeTMonadTransControl = function (dictFunctor) {
    return function (dictMonadRec) {
        return new MonadTransControl(dictMonadRec.Monad0, function () {
            return Control_Monad_Free_Trans.monadTransFreeT(dictFunctor);
        }, function (f) {
            return Control_Monad_Free_Trans.freeT(function (v) {
                return Data_Functor.map((((dictMonadRec.Monad0()).Bind1()).Apply0()).Functor0())(Data_Either.Left.create)(f(function (x) {
                    return Data_Functor.map((((dictMonadRec.Monad0()).Bind1()).Apply0()).Functor0())(eitherToFreeTStT)(Control_Monad_Free_Trans.resume(dictFunctor)(dictMonadRec)(x));
                }));
            });
        }, function (x) {
            return Control_Monad_Free_Trans.freeT(function (v) {
                return Data_Functor.map((((dictMonadRec.Monad0()).Bind1()).Apply0()).Functor0())(freeTStTToEither)(x);
            });
        });
    };
};
var eitherMonadBaseControl = new MonadBaseControl(function () {
    return Control_Monad_Base.eitherMonadBase;
}, function (f) {
    return f(Data_Functor.map(Data_Either.functorEither)(Data_Identity.Identity));
}, Data_Functor.map(Data_Either.functorEither)(runIdentity));
var effMonadBaseControl = new MonadBaseControl(function () {
    return Control_Monad_Base.effMonadBase;
}, function (f) {
    return f(Data_Functor.map(Effect.functorEffect)(Data_Identity.Identity));
}, Data_Functor.map(Effect.functorEffect)(runIdentity));
var defaultRestoreM = function (dictMonadBaseControl) {
    return function (dictMonad) {
        return function (dictMonad1) {
            return function (dictMonadTrans) {
                return function (dictMonadTransControl) {
                    return function (x) {
                        return restoreT(dictMonadTransControl)(restoreM(dictMonadBaseControl)(Data_Functor.map(((dictMonad1.Bind1()).Apply0()).Functor0())(runCompose)(x)));
                    };
                };
            };
        };
    };
};
var defaultLiftBaseWith = function (dictMonadBaseControl) {
    return function (dictMonad) {
        return function (dictMonad1) {
            return function (dictMonadTrans) {
                return function (dictMonadTransControl) {
                    return function (f) {
                        return liftWith(dictMonadTransControl)(function (run) {
                            return liftBaseWith(dictMonadBaseControl)(function (runInBase) {
                                return f(function (x) {
                                    return Data_Functor.map(((dictMonad1.Bind1()).Apply0()).Functor0())(Data_Functor_Compose.Compose)(runInBase(run(x)));
                                });
                            });
                        });
                    };
                };
            };
        };
    };
};
var exceptTMonadBaseControl = function (dictMonadBaseControl) {
    return function (dictMonad) {
        return function (dictMonad1) {
            return new MonadBaseControl(function () {
                return Control_Monad_Base.exceptTMonadBase(dictMonadBaseControl.MonadBase0())(dictMonad)(dictMonad1);
            }, defaultLiftBaseWith(dictMonadBaseControl)(dictMonad)(dictMonad1)(Control_Monad_Except_Trans.monadTransExceptT)(exceptTMonadTransControl(dictMonad)), defaultRestoreM(dictMonadBaseControl)(dictMonad)(dictMonad1)(Control_Monad_Except_Trans.monadTransExceptT)(exceptTMonadTransControl(dictMonad)));
        };
    };
};
var freeTMonadBaseControl = function (dictMonadBaseControl) {
    return function (dictMonad) {
        return function (dictFunctor) {
            return function (dictMonadRec) {
                return new MonadBaseControl(function () {
                    return Control_Monad_Base.freeTMonadBase(dictMonadBaseControl.MonadBase0())((dictMonadBaseControl.MonadBase0()).Monad1())(dictMonad)(dictFunctor);
                }, defaultLiftBaseWith(dictMonadBaseControl)((dictMonadBaseControl.MonadBase0()).Monad1())(dictMonad)(Control_Monad_Free_Trans.monadTransFreeT(dictFunctor))(freeTMonadTransControl(dictFunctor)(dictMonadRec)), defaultRestoreM(dictMonadBaseControl)((dictMonadBaseControl.MonadBase0()).Monad1())(dictMonad)(Control_Monad_Free_Trans.monadTransFreeT(dictFunctor))(freeTMonadTransControl(dictFunctor)(dictMonadRec)));
            };
        };
    };
};

// instance listTMonadBaseControl :: (MonadBaseControl base m stM, Monad m, Monad base) => MonadBaseControl base (ListT m) (Compose stM Identity) where
//   liftBaseWith = defaultLiftBaseWith
//   restoreM = defaultRestoreM
var maybeTMonadBaseControl = function (dictMonadBaseControl) {
    return function (dictMonad) {
        return function (dictMonad1) {
            return new MonadBaseControl(function () {
                return Control_Monad_Base.maybeTMonadBase(dictMonadBaseControl.MonadBase0())(dictMonad)(dictMonad1);
            }, defaultLiftBaseWith(dictMonadBaseControl)(dictMonad)(dictMonad1)(Control_Monad_Maybe_Trans.monadTransMaybeT)(maybeTMonadTransControl(dictMonad)), defaultRestoreM(dictMonadBaseControl)(dictMonad)(dictMonad1)(Control_Monad_Maybe_Trans.monadTransMaybeT)(maybeTMonadTransControl(dictMonad)));
        };
    };
};
var readerTMonadBaseControl = function (dictMonadBaseControl) {
    return function (dictMonad) {
        return function (dictMonad1) {
            return new MonadBaseControl(function () {
                return Control_Monad_Base.readerTMonadBase(dictMonadBaseControl.MonadBase0())(dictMonad)(dictMonad1);
            }, defaultLiftBaseWith(dictMonadBaseControl)(dictMonad)(dictMonad1)(Control_Monad_Reader_Trans.monadTransReaderT)(readerTMonadTransControl(dictMonad)), defaultRestoreM(dictMonadBaseControl)(dictMonad)(dictMonad1)(Control_Monad_Reader_Trans.monadTransReaderT)(readerTMonadTransControl(dictMonad)));
        };
    };
};
var rwsTMonadBaseControl = function (dictMonadBaseControl) {
    return function (dictMonad) {
        return function (dictMonad1) {
            return function (dictMonoid) {
                return new MonadBaseControl(function () {
                    return Control_Monad_Base.rwsTMonadBase(dictMonadBaseControl.MonadBase0())(dictMonad)(dictMonad1)(dictMonoid);
                }, defaultLiftBaseWith(dictMonadBaseControl)(dictMonad)(dictMonad1)(Control_Monad_RWS_Trans.monadTransRWST(dictMonoid))(rwsTMonadTransControl(dictMonoid)(dictMonad)), defaultRestoreM(dictMonadBaseControl)(dictMonad)(dictMonad1)(Control_Monad_RWS_Trans.monadTransRWST(dictMonoid))(rwsTMonadTransControl(dictMonoid)(dictMonad)));
            };
        };
    };
};
var stateTMonadBaseControl = function (dictMonadBaseControl) {
    return function (dictMonad) {
        return function (dictMonad1) {
            return new MonadBaseControl(function () {
                return Control_Monad_Base.stateTMonadBase(dictMonadBaseControl.MonadBase0())(dictMonad)(dictMonad1);
            }, defaultLiftBaseWith(dictMonadBaseControl)(dictMonad)(dictMonad1)(Control_Monad_State_Trans.monadTransStateT)(stateTMonadTransControl(dictMonad)), defaultRestoreM(dictMonadBaseControl)(dictMonad)(dictMonad1)(Control_Monad_State_Trans.monadTransStateT)(stateTMonadTransControl(dictMonad)));
        };
    };
};
var writerTMonadBaseControl = function (dictMonadBaseControl) {
    return function (dictMonad) {
        return function (dictMonad1) {
            return function (dictMonoid) {
                return new MonadBaseControl(function () {
                    return Control_Monad_Base.writerTMonadBase(dictMonadBaseControl.MonadBase0())(dictMonad)(dictMonad1)(dictMonoid);
                }, defaultLiftBaseWith(dictMonadBaseControl)(dictMonad)(dictMonad1)(Control_Monad_Writer_Trans.monadTransWriterT(dictMonoid))(writerTMonadTransControl(dictMonoid)(dictMonad)), defaultRestoreM(dictMonadBaseControl)(dictMonad)(dictMonad1)(Control_Monad_Writer_Trans.monadTransWriterT(dictMonoid))(writerTMonadTransControl(dictMonoid)(dictMonad)));
            };
        };
    };
};
var arrayMonadBaseControl = new MonadBaseControl(function () {
    return Control_Monad_Base.arrayMonadBase;
}, function (f) {
    return f(Data_Functor.map(Data_Functor.functorArray)(Data_Identity.Identity));
}, Data_Functor.map(Data_Functor.functorArray)(runIdentity));
var affMonadBaseControl = new MonadBaseControl(function () {
    return Control_Monad_Base.affMonadBase;
}, function (f) {
    return f(Data_Functor.map(Effect_Aff.functorAff)(Data_Identity.Identity));
}, Data_Functor.map(Effect_Aff.functorAff)(runIdentity));
module.exports = {
    MonadTransControl: MonadTransControl,
    liftWith: liftWith,
    restoreT: restoreT,
    integrateT: integrateT,
    MonadBaseControl: MonadBaseControl,
    liftBaseWith: liftBaseWith,
    restoreM: restoreM,
    integrateM: integrateM,
    defaultLiftBaseWith: defaultLiftBaseWith,
    defaultRestoreM: defaultRestoreM,
    WriterTStT: WriterTStT,
    writerTStTToTuple: writerTStTToTuple,
    tupleToWriterTStT: tupleToWriterTStT,
    FreeTStT: FreeTStT,
    freeTStTToEither: freeTStTToEither,
    eitherToFreeTStT: eitherToFreeTStT,
    readerTMonadTransControl: readerTMonadTransControl,
    functorWriterTStT: functorWriterTStT,
    functorFreeTStT: functorFreeTStT,
    freeTMonadTransControl: freeTMonadTransControl,
    writerTMonadTransControl: writerTMonadTransControl,
    stateTMonadTransControl: stateTMonadTransControl,
    exceptTMonadTransControl: exceptTMonadTransControl,
    maybeTMonadTransControl: maybeTMonadTransControl,
    rwsTMonadTransControl: rwsTMonadTransControl,
    affMonadBaseControl: affMonadBaseControl,
    effMonadBaseControl: effMonadBaseControl,
    eitherMonadBaseControl: eitherMonadBaseControl,
    tupleMonadBaseControl: tupleMonadBaseControl,
    maybeMonadBaseControl: maybeMonadBaseControl,
    identityMonadBaseControl: identityMonadBaseControl,
    listMonadBaseControl: listMonadBaseControl,
    arrayMonadBaseControl: arrayMonadBaseControl,
    funcMonadBaseControl: funcMonadBaseControl,
    readerTMonadBaseControl: readerTMonadBaseControl,
    writerTMonadBaseControl: writerTMonadBaseControl,
    stateTMonadBaseControl: stateTMonadBaseControl,
    exceptTMonadBaseControl: exceptTMonadBaseControl,
    maybeTMonadBaseControl: maybeTMonadBaseControl,
    rwsTMonadBaseControl: rwsTMonadBaseControl,
    freeTMonadBaseControl: freeTMonadBaseControl
};
