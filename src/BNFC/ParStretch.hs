{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module BNFC.ParStretch where
import BNFC.AbsStretch
import BNFC.LexStretch
import BNFC.ErrM
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn27 :: (Integer) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> (Integer)
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: (String) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> (String)
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: (Ident) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> (Ident)
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: (Program) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> (Program)
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: (Boolean) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> (Boolean)
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: (Literal) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> (Literal)
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: (Type) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> (Type)
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: ([Type]) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> ([Type])
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: (Stm) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> (Stm)
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: ([Stm]) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> ([Stm])
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: (Block) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> (Block)
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: (BlockExp) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> (BlockExp)
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyIn39 :: ([BlockExp]) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> ([BlockExp])
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: (Exp) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> (Exp)
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: (Exp) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> (Exp)
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyIn42 :: (Exp) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> (Exp)
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
happyIn43 :: (Exp) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> (Exp)
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
happyIn44 :: (Exp) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> (Exp)
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
happyIn45 :: (Exp) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> (Exp)
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
happyIn46 :: (Exp) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> (Exp)
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
happyIn47 :: (Exp) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> (Exp)
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
happyIn48 :: ([Exp]) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> ([Exp])
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
happyIn49 :: (AnonFunc) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> (AnonFunc)
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
happyIn50 :: (MemberInit) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> (MemberInit)
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
happyIn51 :: ([MemberInit]) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> ([MemberInit])
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
happyIn52 :: (FormalArg) -> (HappyAbsSyn )
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> (FormalArg)
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
happyIn53 :: ([FormalArg]) -> (HappyAbsSyn )
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> ([FormalArg])
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x04\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x01\xc1\x00\x00\x00\x00\x00\x00\x00\x04\x00\x18\x12\x40\x00\x00\x00\x00\x00\x00\x00\x01\x00\x86\x04\x10\x00\x00\x00\x00\x00\x00\x48\x08\x00\xdc\x7e\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x60\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x12\x02\x00\x25\xdf\x01\x00\x00\x00\x00\x00\x80\x84\x00\x40\xc9\x77\x00\x00\x00\x00\x00\x00\x20\x21\x00\x50\xf2\x1d\x00\x00\x00\x00\x00\x00\x48\x08\x00\x94\x7c\x07\x00\x00\x00\x00\x00\x00\x12\x02\x00\x25\xdf\x01\x00\x00\x00\x00\x00\x80\x84\x00\x40\xc9\x77\x00\x00\x00\x00\x00\x00\x20\x21\x00\x50\xf2\x1d\x00\x00\x00\x00\x00\x00\x40\x00\x00\x94\x7c\x07\x00\x00\x00\x00\x00\x00\x12\x02\x00\x25\xdf\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\xd8\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x81\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x40\xc9\x77\x00\x00\x00\x00\x00\x00\x20\x23\x00\x50\xf2\x1d\x00\x00\x00\x00\x00\x00\x40\x00\x00\x94\x7c\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x84\x00\x40\xc9\x77\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x12\x02\x00\x25\xdf\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x80\x1d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x48\x08\x00\xdc\x7e\x07\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x48\x08\x00\xdc\x7e\x07\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x86\x04\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x60\x48\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x48\x08\x00\x94\x7c\x07\x00\x00\x00\x00\x00\x00\x12\x02\x00\x25\xdf\x01\x00\x00\x00\x00\x00\x80\x84\x00\x40\xc9\x77\x00\x00\x00\x00\x00\x00\x20\x21\x00\x50\xf2\x1d\x00\x00\x00\x00\x00\x00\x48\x08\x00\x94\x7c\x07\x00\x00\x00\x00\x00\x00\x12\x02\x00\x25\xdf\x01\x00\x00\x00\x00\x00\x80\x84\x00\x40\xc9\x77\x00\x00\x00\x00\x00\x00\x20\x21\x00\x50\xf2\x1d\x00\x00\x00\x00\x00\x00\x48\x08\x00\x94\x7c\x07\x00\x00\x00\x00\x00\x00\x12\x02\x00\x25\xdf\x01\x00\x00\x00\x00\x00\x80\x84\x00\x40\xc9\x77\x00\x00\x00\x00\x00\x00\x20\x21\x00\x50\xf2\x1d\x00\x00\x00\x00\x00\x00\x48\x08\x00\x94\x7c\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x20\x21\x00\x70\xfb\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x12\x02\x00\x25\xdf\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x81\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x84\x00\x40\xc9\x77\x00\x00\x00\x00\x00\x00\x20\x21\x00\x50\xf2\x1d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x20\x21\x00\x50\xf2\x1d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x10\x00\x60\x48\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x41\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x76\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x01\x00\x86\x04\x10\x00\x00\x00\x00\x00\x00\x48\x08\x00\x94\x7c\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x12\x02\x00\x25\xdf\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x80\x21\x01\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x80\x21\x01\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram","%start_pBoolean","%start_pLiteral","%start_pType","%start_pListType","%start_pStm","%start_pListStm","%start_pBlock","%start_pBlockExp","%start_pListBlockExp","%start_pExp","%start_pExp1","%start_pExp2","%start_pExp3","%start_pExp4","%start_pExp5","%start_pExp6","%start_pExp7","%start_pListExp","%start_pAnonFunc","%start_pMemberInit","%start_pListMemberInit","%start_pFormalArg","%start_pListFormalArg","Integer","String","Ident","Program","Boolean","Literal","Type","ListType","Stm","ListStm","Block","BlockExp","ListBlockExp","Exp","Exp1","Exp2","Exp3","Exp4","Exp5","Exp6","Exp7","ListExp","AnonFunc","MemberInit","ListMemberInit","FormalArg","ListFormalArg","'!'","'!='","'&&'","'('","')'","'*'","'+'","','","'-'","'->'","'.'","'/'","':'","';'","'<'","'<='","'='","'=='","'>'","'>='","'Fn'","'bool'","'else'","'false'","'fn'","'if'","'int'","'let'","'print'","'string'","'struct'","'true'","'while'","'{'","'|'","'||'","'}'","L_integ","L_quoted","L_ident","%eof"]
        bit_start = st * 94
        bit_end = (st + 1) * 94
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..93]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\xef\xff\xb0\x00\x9d\x00\x9d\x00\x3b\x00\x00\x00\x23\x00\x27\x00\x00\x00\x75\x00\x75\x00\x75\x00\x75\x00\x75\x00\x75\x00\x75\x00\x88\x00\x75\x00\x5d\x01\xe9\xff\xe9\xff\xe9\xff\xe9\xff\x29\x00\x00\x00\x6d\x00\x45\x00\x43\x00\x00\x00\x43\x00\x77\x00\x92\x00\x54\x00\x54\x00\x54\x00\x91\x00\x95\x00\x00\x00\x00\x00\x55\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc4\x00\x9f\x00\xe2\x00\xc9\x03\xcc\x00\x0a\x00\x00\x00\x07\x00\xb3\x00\x00\x00\x88\x00\x4d\x00\x88\x00\x00\x00\x75\x00\xea\x00\x00\x00\x75\x00\x00\x00\x00\x00\xd1\x00\x08\x00\xcd\x00\xfd\xff\x37\x00\x76\x00\x05\x00\x67\x00\xcd\x00\xec\xff\xcd\x00\xcd\x00\x01\x00\xf6\xff\xbb\x00\xeb\x00\xd4\x00\xd4\x00\xd4\x00\x00\x00\x05\x01\xe7\x00\x0e\x01\x18\x01\x00\x00\x00\x00\x00\x00\xfb\x00\xfb\x00\x22\x01\x01\x01\x01\x01\x3b\x00\x2d\x01\x00\x00\x9d\x00\x00\x00\x9d\x00\x1c\x01\x3f\x00\x3d\x01\x00\x00\x00\x00\x00\x00\x75\x00\x75\x00\x75\x00\x75\x00\x75\x00\x75\x00\x75\x00\x75\x00\x75\x00\x75\x00\x75\x00\x75\x00\x75\x00\x2a\x01\x2a\x01\x13\x00\x28\x01\x75\x00\x28\x01\x07\x00\x50\x01\x07\x00\x75\x00\x75\x00\x00\x00\x46\x01\x30\x01\x75\x00\x30\x01\x9d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4a\x01\x00\x00\x00\x00\x00\x00\x58\x01\x73\x01\x00\x00\xf3\xff\x00\x00\x61\x01\x00\x00\x8a\x01\x00\x00\x00\x00\x0a\x00\x0a\x00\xcc\x00\xcc\x00\xcc\x00\xcc\x00\xcc\x00\xcc\x00\xc9\x03\x94\x01\x55\x01\x9d\x00\x75\x00\x55\x01\x00\x00\x8f\x01\x8e\x01\x74\x01\x8c\x01\x95\x01\x98\x01\x00\x00\x00\x00\x00\x00\x00\x00\x89\x01\x00\x00\x00\x00\x5f\x00\x75\x00\x00\x00\x00\x00\x9d\x00\x00\x00\xa0\x01\x00\x00\x9d\x00\x8d\x01\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x3a\x00\xac\x01\x46\x00\xab\x00\x1f\x01\xd9\x00\xab\x01\xa7\x01\x89\x00\xb1\x01\x63\x01\x32\x02\x49\x02\x77\x02\xa5\x02\x46\x03\x8b\x03\xb2\x03\xf0\x00\xaf\x01\x70\x00\x8c\x00\x40\x00\x62\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x66\x00\xb8\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbf\x03\x7a\x01\xcc\x03\x00\x00\x91\x01\x00\x00\x00\x00\xa8\x01\xbd\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x78\x01\x00\x00\x00\x00\x07\x01\x00\x00\x00\x00\x00\x00\xc5\x01\xc6\x01\xc9\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x07\x01\x00\x00\x00\x00\x4d\x01\x00\x00\x64\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x02\x8e\x02\xbc\x02\xd3\x02\xea\x02\x01\x03\x18\x03\x2f\x03\x5d\x03\x74\x03\x98\x03\xa5\x03\x1e\x01\xd2\x01\x8f\x00\x35\x01\xcf\x01\xbf\x01\xd3\x01\x00\x00\x00\x00\x00\x00\x4c\x01\xd6\x01\x00\x00\x00\x00\xe0\x00\xed\x01\x69\x00\xba\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd4\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9c\x00\xd2\x00\x04\x02\xb8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd5\x01\x00\x00\x00\x00\xd8\x01\x1b\x02\x00\x00\x00\x00\x08\x01\x00\x00\x00\x00\x00\x00\x36\x01\xe1\x01\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xcc\xff\x00\x00\x00\x00\x00\x00\xd7\xff\x00\x00\xcc\xff\x00\x00\x00\x00\xc4\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa3\xff\x00\x00\x00\x00\x9d\xff\x00\x00\x99\xff\x00\x00\xe7\xff\x00\x00\x98\xff\x00\x00\xe5\xff\x00\x00\x00\x00\x9c\xff\x00\x00\x00\x00\x00\x00\x99\xff\x00\x00\xdf\xff\xde\xff\xab\xff\xe0\xff\xac\xff\xc8\xff\xa6\xff\xa2\xff\xc1\xff\xbf\xff\xbd\xff\xb6\xff\xb3\xff\xb0\xff\xad\xff\x00\x00\xa5\xff\x00\x00\x00\x00\x00\x00\xe2\xff\x00\x00\x00\x00\xe3\xff\x00\x00\xcc\xff\xe6\xff\xab\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcf\xff\x00\x00\x00\x00\x00\x00\x00\x00\xdd\xff\xd6\xff\x00\x00\x00\x00\x00\x00\xdb\xff\xda\xff\xd9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe4\xff\xcb\xff\xe1\xff\xd7\xff\xdc\xff\xd7\xff\x00\x00\x00\x00\x00\x00\xce\xff\xcd\xff\xc3\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa3\xff\x00\x00\x9d\xff\x00\x00\x00\x00\x00\x00\x00\x00\xae\xff\x00\x00\xaf\xff\xa3\xff\x00\x00\xa0\xff\x00\x00\x9d\xff\x00\x00\x99\xff\x00\x00\x9a\xff\x97\xff\x9e\xff\x9b\xff\x00\x00\xc2\xff\xa1\xff\xa4\xff\xc7\xff\x00\x00\xc5\xff\x00\x00\xca\xff\x00\x00\xa7\xff\x00\x00\xb1\xff\xb2\xff\xb4\xff\xb5\xff\xb7\xff\xb8\xff\xbc\xff\xb9\xff\xba\xff\xbb\xff\xbe\xff\xc0\xff\x99\xff\x00\x00\x00\x00\x99\xff\xd5\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa9\xff\xaa\xff\xc9\xff\xa8\xff\x00\x00\x9f\xff\xc6\xff\x00\x00\x00\x00\xd1\xff\xd2\xff\x00\x00\xd8\xff\x00\x00\xd4\xff\x00\x00\x00\x00\xd0\xff\xd3\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x0e\x00\x01\x00\x06\x00\x0e\x00\x04\x00\x1a\x00\x18\x00\x03\x00\x0c\x00\x09\x00\x04\x00\x04\x00\x21\x00\x22\x00\x20\x00\x06\x00\x28\x00\x0b\x00\x0b\x00\x01\x00\x29\x00\x0c\x00\x04\x00\x25\x00\x18\x00\x19\x00\x1a\x00\x09\x00\x1c\x00\x1d\x00\x29\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x29\x00\x26\x00\x27\x00\x28\x00\x29\x00\x18\x00\x19\x00\x1a\x00\x29\x00\x1c\x00\x1d\x00\x29\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x01\x00\x03\x00\x07\x00\x04\x00\x09\x00\x1a\x00\x02\x00\x09\x00\x09\x00\x22\x00\x00\x00\x01\x00\x21\x00\x22\x00\x04\x00\x05\x00\x0d\x00\x08\x00\x01\x00\x26\x00\x11\x00\x04\x00\x05\x00\x18\x00\x19\x00\x1a\x00\x09\x00\x1c\x00\x1d\x00\x19\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x29\x00\x26\x00\x27\x00\x28\x00\x02\x00\x18\x00\x11\x00\x1a\x00\x02\x00\x0a\x00\x1d\x00\x02\x00\x29\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x02\x00\x26\x00\x27\x00\x28\x00\x01\x00\x22\x00\x02\x00\x04\x00\x0d\x00\x19\x00\x1a\x00\x29\x00\x09\x00\x19\x00\x1a\x00\x22\x00\x19\x00\x1a\x00\x0d\x00\x0f\x00\x10\x00\x17\x00\x12\x00\x13\x00\x14\x00\x24\x00\x04\x00\x18\x00\x02\x00\x1a\x00\x29\x00\x02\x00\x1d\x00\x0a\x00\x0b\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x08\x00\x26\x00\x27\x00\x28\x00\x02\x00\x29\x00\x18\x00\x04\x00\x1a\x00\x17\x00\x18\x00\x1d\x00\x17\x00\x18\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x02\x00\x26\x00\x27\x00\x28\x00\x06\x00\x15\x00\x16\x00\x04\x00\x19\x00\x1a\x00\x22\x00\x1b\x00\x28\x00\x02\x00\x1e\x00\x02\x00\x02\x00\x03\x00\x04\x00\x06\x00\x06\x00\x07\x00\x24\x00\x09\x00\x28\x00\x0b\x00\x0c\x00\x18\x00\x0e\x00\x0f\x00\x10\x00\x08\x00\x12\x00\x13\x00\x14\x00\x20\x00\x19\x00\x1a\x00\x07\x00\x02\x00\x09\x00\x26\x00\x27\x00\x06\x00\x00\x00\x01\x00\x02\x00\x29\x00\x04\x00\x05\x00\x24\x00\x25\x00\x08\x00\x02\x00\x0a\x00\x0b\x00\x03\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x04\x00\x16\x00\x00\x00\x01\x00\x02\x00\x22\x00\x04\x00\x05\x00\x29\x00\x17\x00\x18\x00\x0e\x00\x0a\x00\x0b\x00\x28\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x02\x00\x04\x00\x05\x00\x08\x00\x06\x00\x08\x00\x29\x00\x0a\x00\x0b\x00\x05\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x04\x00\x16\x00\x00\x00\x01\x00\x02\x00\x02\x00\x04\x00\x05\x00\x29\x00\x06\x00\x07\x00\x05\x00\x0a\x00\x0b\x00\x29\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x02\x00\x04\x00\x05\x00\x0e\x00\x06\x00\x08\x00\x22\x00\x0a\x00\x0b\x00\x04\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x22\x00\x16\x00\x00\x00\x01\x00\x02\x00\x02\x00\x04\x00\x05\x00\x28\x00\x06\x00\x07\x00\x05\x00\x0a\x00\x0b\x00\x28\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x02\x00\x04\x00\x05\x00\x23\x00\x06\x00\x07\x00\x22\x00\x0a\x00\x0b\x00\x17\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x05\x00\x16\x00\x00\x00\x01\x00\x02\x00\x28\x00\x04\x00\x05\x00\x23\x00\x24\x00\x0a\x00\x0b\x00\x0a\x00\x0b\x00\x25\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x05\x00\x16\x00\x00\x00\x01\x00\x02\x00\x05\x00\x04\x00\x05\x00\x03\x00\x0a\x00\x25\x00\x0e\x00\x0a\x00\x0b\x00\x05\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x11\x00\x16\x00\x00\x00\x01\x00\x02\x00\x22\x00\x04\x00\x05\x00\x0e\x00\x22\x00\x04\x00\x0a\x00\x0a\x00\x0b\x00\x09\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x0c\x00\x16\x00\x00\x00\x01\x00\x02\x00\x0a\x00\x04\x00\x05\x00\x16\x00\x09\x00\x02\x00\x02\x00\x0a\x00\x0b\x00\x02\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x02\x00\x16\x00\x00\x00\x01\x00\x02\x00\x0a\x00\x04\x00\x05\x00\xff\xff\x0a\x00\x0a\x00\x0a\x00\x0a\x00\x0b\x00\x0a\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x0a\x00\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\xff\xff\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\xff\xff\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\xff\xff\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\x13\x00\x14\x00\xff\xff\x16\x00\x0a\x00\x0b\x00\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\x13\x00\x14\x00\xff\xff\x16\x00\x0a\x00\x0b\x00\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\x13\x00\x14\x00\xff\xff\x16\x00\x0a\x00\x0b\x00\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\xff\xff\x14\x00\xff\xff\x16\x00\x0a\x00\x0b\x00\x02\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\xff\xff\x14\x00\xff\xff\x16\x00\x0a\x00\x0b\x00\x0f\x00\x10\x00\xff\xff\x12\x00\x13\x00\x14\x00\xff\xff\xff\xff\x14\x00\xff\xff\x16\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x6b\x00\x38\x00\x78\x00\x6c\x00\x39\x00\x3c\x00\x3b\x00\x6f\x00\x79\x00\x3a\x00\x7a\x00\x7a\x00\x3f\x00\x40\x00\x3e\x00\x78\x00\x1e\x00\x7b\x00\x7b\x00\x38\x00\xff\xff\x79\x00\x39\x00\xb5\x00\x3b\x00\x52\x00\x3c\x00\x3a\x00\x53\x00\x3d\x00\xff\xff\x54\x00\x3e\x00\x3f\x00\x40\x00\x25\x00\x26\x00\xff\xff\x1a\x00\x41\x00\x1e\x00\xff\xff\x3b\x00\x52\x00\x3c\x00\xff\xff\x53\x00\x3d\x00\xff\xff\x54\x00\x3e\x00\x3f\x00\x40\x00\x25\x00\x26\x00\x98\x00\x1a\x00\x41\x00\x1e\x00\x38\x00\x60\x00\x76\x00\x39\x00\x77\x00\x3c\x00\x1a\x00\x61\x00\x3a\x00\x40\x00\x26\x00\x27\x00\x3f\x00\x40\x00\x29\x00\x5d\x00\xa9\x00\x8a\x00\x38\x00\x1a\x00\xaa\x00\x39\x00\x64\x00\x3b\x00\x52\x00\x3c\x00\x3a\x00\x53\x00\x3d\x00\x1e\x00\x54\x00\x3e\x00\x3f\x00\x40\x00\x25\x00\x26\x00\xff\xff\x1a\x00\x41\x00\x1e\x00\x1a\x00\x3b\x00\x85\x00\x3c\x00\x1a\x00\xc2\x00\x3d\x00\x1a\x00\xff\xff\x3e\x00\x3f\x00\x40\x00\x25\x00\x26\x00\x1f\x00\x1a\x00\x41\x00\x1e\x00\x38\x00\x7c\x00\x70\x00\x39\x00\x8b\x00\x1b\x00\x1c\x00\xff\xff\x3a\x00\x1b\x00\x86\x00\x40\x00\x1b\x00\x8c\x00\x89\x00\x71\x00\x72\x00\x22\x00\x73\x00\x74\x00\x75\x00\x6e\x00\x39\x00\x3b\x00\x1f\x00\x3c\x00\xff\xff\x1f\x00\x3d\x00\x2b\x00\x4b\x00\x3e\x00\x3f\x00\x40\x00\x25\x00\x26\x00\x88\x00\x1a\x00\x41\x00\x1e\x00\x1a\x00\xff\xff\x3b\x00\x58\x00\x3c\x00\x20\x00\x21\x00\x3d\x00\x20\x00\x98\x00\x3e\x00\x3f\x00\x40\x00\x25\x00\x26\x00\x54\x00\x1a\x00\x41\x00\x1e\x00\x5c\x00\x59\x00\x5a\x00\x5f\x00\x1b\x00\xb1\x00\x40\x00\x5b\x00\x1e\x00\x1a\x00\x5c\x00\x54\x00\xa6\xff\xa6\xff\xa6\xff\x8b\x00\xa6\xff\xa6\xff\x6e\x00\xa6\xff\x1e\x00\xa6\xff\xa6\xff\x3b\x00\xa6\xff\xa6\xff\xa6\xff\x84\x00\xa6\xff\xa6\xff\xa6\xff\x3e\x00\x1b\x00\xae\x00\x76\x00\x54\x00\x77\x00\x1a\x00\x41\x00\xb0\x00\x26\x00\x27\x00\x28\x00\xff\xff\x29\x00\x2a\x00\xa6\xff\xa6\xff\x4e\x00\x1f\x00\x2b\x00\x4f\x00\x6f\x00\x50\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x7f\x00\x36\x00\x26\x00\x27\x00\x28\x00\x7c\x00\x29\x00\x2a\x00\xff\xff\x20\x00\x8e\x00\x6b\x00\x2b\x00\x2c\x00\x1e\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x26\x00\x27\x00\x28\x00\x54\x00\x29\x00\x2a\x00\x67\x00\xbe\x00\x62\x00\xff\xff\x2b\x00\x4f\x00\x66\x00\x50\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x65\x00\x36\x00\x26\x00\x27\x00\x28\x00\x54\x00\x29\x00\x2a\x00\xff\xff\x55\x00\x56\x00\x64\x00\x2b\x00\x2c\x00\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x9a\x00\x36\x00\x26\x00\x27\x00\x28\x00\x54\x00\x29\x00\x2a\x00\x6c\x00\xc2\x00\x62\x00\xab\x00\x2b\x00\x4f\x00\xa8\x00\x96\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x40\x00\x36\x00\x26\x00\x27\x00\x28\x00\x54\x00\x29\x00\x2a\x00\x1e\x00\x55\x00\xac\x00\x93\x00\x2b\x00\x2c\x00\x1e\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x91\x00\x36\x00\x26\x00\x27\x00\x28\x00\x54\x00\x29\x00\x2a\x00\x90\x00\x55\x00\xab\x00\x40\x00\x2b\x00\x2c\x00\xb7\x00\x49\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\xb6\x00\x36\x00\x26\x00\x27\x00\x28\x00\x1e\x00\x29\x00\x2a\x00\x25\x00\x26\x00\x2b\x00\x6c\x00\x2b\x00\x2c\x00\xb4\x00\x81\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\xb3\x00\x36\x00\x26\x00\x27\x00\x28\x00\xae\x00\x29\x00\x2a\x00\x6f\x00\xbe\x00\xbd\x00\xbc\x00\x2b\x00\x2c\x00\xba\x00\x7f\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\xbb\x00\x36\x00\x26\x00\x27\x00\x28\x00\x40\x00\x29\x00\x2a\x00\xc4\x00\x40\x00\x5f\x00\x4c\x00\x2b\x00\x2c\x00\x4d\x00\x7d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x4a\x00\x36\x00\x26\x00\x27\x00\x28\x00\x85\x00\x29\x00\x2a\x00\x23\x00\x7c\x00\x69\x00\x68\x00\x2b\x00\x2c\x00\x67\x00\x94\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x99\x00\x36\x00\x26\x00\x27\x00\x28\x00\x95\x00\x29\x00\x2a\x00\x00\x00\x93\x00\xb7\x00\xb8\x00\x2b\x00\x2c\x00\xc0\x00\x90\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\xc4\x00\x36\x00\x26\x00\x27\x00\x28\x00\x00\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x2c\x00\x00\x00\x8d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x36\x00\x26\x00\x27\x00\x28\x00\x00\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x2c\x00\x00\x00\xaf\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x36\x00\x26\x00\x27\x00\x28\x00\x00\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x2c\x00\x00\x00\xbf\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x36\x00\x26\x00\x27\x00\x41\x00\x00\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x48\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x36\x00\x26\x00\x27\x00\x41\x00\x00\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x47\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x36\x00\x26\x00\x27\x00\x41\x00\x00\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\xa6\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x36\x00\x26\x00\x27\x00\x41\x00\x00\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x36\x00\x26\x00\x27\x00\x41\x00\x00\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa5\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x36\x00\x26\x00\x27\x00\x41\x00\x00\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x45\x00\x32\x00\x33\x00\x34\x00\x00\x00\x36\x00\x26\x00\x27\x00\x41\x00\x00\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa4\x00\x32\x00\x33\x00\x34\x00\x00\x00\x36\x00\x26\x00\x27\x00\x41\x00\x00\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa3\x00\x32\x00\x33\x00\x34\x00\x00\x00\x36\x00\x26\x00\x27\x00\x41\x00\x00\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa2\x00\x32\x00\x33\x00\x34\x00\x00\x00\x36\x00\x26\x00\x27\x00\x41\x00\x00\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa1\x00\x32\x00\x33\x00\x34\x00\x00\x00\x36\x00\x26\x00\x27\x00\x41\x00\x00\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x00\x32\x00\x33\x00\x34\x00\x00\x00\x36\x00\x26\x00\x27\x00\x41\x00\x00\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9f\x00\x32\x00\x33\x00\x34\x00\x00\x00\x36\x00\x26\x00\x27\x00\x41\x00\x00\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x00\x33\x00\x34\x00\x00\x00\x36\x00\x26\x00\x27\x00\x41\x00\x00\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9e\x00\x33\x00\x34\x00\x00\x00\x36\x00\x26\x00\x27\x00\x41\x00\x00\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9d\x00\x33\x00\x34\x00\x00\x00\x36\x00\x26\x00\x27\x00\x41\x00\x00\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x2c\x00\x00\x00\x26\x00\x27\x00\x41\x00\x00\x00\x29\x00\x2a\x00\x43\x00\x34\x00\x00\x00\x36\x00\x2b\x00\x2c\x00\x00\x00\x26\x00\x27\x00\x41\x00\x00\x00\x29\x00\x2a\x00\x9c\x00\x34\x00\x00\x00\x36\x00\x2b\x00\x2c\x00\x00\x00\x26\x00\x27\x00\x41\x00\x00\x00\x29\x00\x2a\x00\x9b\x00\x34\x00\x00\x00\x36\x00\x2b\x00\x2c\x00\x00\x00\x26\x00\x27\x00\x41\x00\x00\x00\x29\x00\x2a\x00\x00\x00\x42\x00\x00\x00\x36\x00\x2b\x00\x2c\x00\x70\x00\x26\x00\x27\x00\x41\x00\x00\x00\x29\x00\x2a\x00\x00\x00\x82\x00\x00\x00\x36\x00\x2b\x00\x2c\x00\x71\x00\x72\x00\x00\x00\x73\x00\x74\x00\x75\x00\x00\x00\x00\x00\x80\x00\x00\x00\x36\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (24, 104) [
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104)
	]

happy_n_terms = 42 :: Int
happy_n_nonterms = 27 :: Int

happyReduce_24 = happySpecReduce_1  0# happyReduction_24
happyReduction_24 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TI happy_var_1)) -> 
	happyIn27
		 (read happy_var_1
	)}

happyReduce_25 = happySpecReduce_1  1# happyReduction_25
happyReduction_25 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TL happy_var_1)) -> 
	happyIn28
		 (happy_var_1
	)}

happyReduce_26 = happySpecReduce_1  2# happyReduction_26
happyReduction_26 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TV happy_var_1)) -> 
	happyIn29
		 (Ident happy_var_1
	)}

happyReduce_27 = happySpecReduce_1  3# happyReduction_27
happyReduction_27 happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	happyIn30
		 (BNFC.AbsStretch.ProgramEntry (reverse happy_var_1)
	)}

happyReduce_28 = happySpecReduce_1  4# happyReduction_28
happyReduction_28 happy_x_1
	 =  happyIn31
		 (BNFC.AbsStretch.Boolean_true
	)

happyReduce_29 = happySpecReduce_1  4# happyReduction_29
happyReduction_29 happy_x_1
	 =  happyIn31
		 (BNFC.AbsStretch.Boolean_false
	)

happyReduce_30 = happySpecReduce_2  5# happyReduction_30
happyReduction_30 happy_x_2
	happy_x_1
	 =  happyIn32
		 (BNFC.AbsStretch.LiteralUnit
	)

happyReduce_31 = happySpecReduce_1  5# happyReduction_31
happyReduction_31 happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn32
		 (BNFC.AbsStretch.LiteralBoolean happy_var_1
	)}

happyReduce_32 = happySpecReduce_1  5# happyReduction_32
happyReduction_32 happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	happyIn32
		 (BNFC.AbsStretch.LiteralInteger happy_var_1
	)}

happyReduce_33 = happySpecReduce_1  5# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	happyIn32
		 (BNFC.AbsStretch.LiteralString happy_var_1
	)}

happyReduce_34 = happySpecReduce_1  6# happyReduction_34
happyReduction_34 happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	happyIn33
		 (BNFC.AbsStretch.TyIdent happy_var_1
	)}

happyReduce_35 = happySpecReduce_2  6# happyReduction_35
happyReduction_35 happy_x_2
	happy_x_1
	 =  happyIn33
		 (BNFC.AbsStretch.TyUnit
	)

happyReduce_36 = happySpecReduce_1  6# happyReduction_36
happyReduction_36 happy_x_1
	 =  happyIn33
		 (BNFC.AbsStretch.TyBool
	)

happyReduce_37 = happySpecReduce_1  6# happyReduction_37
happyReduction_37 happy_x_1
	 =  happyIn33
		 (BNFC.AbsStretch.TyInt
	)

happyReduce_38 = happySpecReduce_1  6# happyReduction_38
happyReduction_38 happy_x_1
	 =  happyIn33
		 (BNFC.AbsStretch.TyString
	)

happyReduce_39 = happyReduce 6# 6# happyReduction_39
happyReduction_39 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut34 happy_x_3 of { happy_var_3 -> 
	case happyOut33 happy_x_6 of { happy_var_6 -> 
	happyIn33
		 (BNFC.AbsStretch.TyFun happy_var_3 happy_var_6
	) `HappyStk` happyRest}}

happyReduce_40 = happySpecReduce_0  7# happyReduction_40
happyReduction_40  =  happyIn34
		 ([]
	)

happyReduce_41 = happySpecReduce_1  7# happyReduction_41
happyReduction_41 happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	happyIn34
		 ((:[]) happy_var_1
	)}

happyReduce_42 = happySpecReduce_3  7# happyReduction_42
happyReduction_42 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	case happyOut34 happy_x_3 of { happy_var_3 -> 
	happyIn34
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_43 = happyReduce 6# 8# happyReduction_43
happyReduction_43 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut29 happy_x_2 of { happy_var_2 -> 
	case happyOut53 happy_x_4 of { happy_var_4 -> 
	case happyOut37 happy_x_6 of { happy_var_6 -> 
	happyIn35
		 (BNFC.AbsStretch.SFunc happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_44 = happyReduce 8# 8# happyReduction_44
happyReduction_44 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut29 happy_x_2 of { happy_var_2 -> 
	case happyOut53 happy_x_4 of { happy_var_4 -> 
	case happyOut33 happy_x_7 of { happy_var_7 -> 
	case happyOut37 happy_x_8 of { happy_var_8 -> 
	happyIn35
		 (BNFC.AbsStretch.SFuncRet happy_var_2 happy_var_4 happy_var_7 happy_var_8
	) `HappyStk` happyRest}}}}

happyReduce_45 = happyReduce 5# 8# happyReduction_45
happyReduction_45 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut29 happy_x_2 of { happy_var_2 -> 
	case happyOut53 happy_x_4 of { happy_var_4 -> 
	happyIn35
		 (BNFC.AbsStretch.SStruct happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_46 = happyReduce 5# 8# happyReduction_46
happyReduction_46 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut29 happy_x_2 of { happy_var_2 -> 
	case happyOut40 happy_x_4 of { happy_var_4 -> 
	happyIn35
		 (BNFC.AbsStretch.SLet happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_47 = happyReduce 7# 8# happyReduction_47
happyReduction_47 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut29 happy_x_2 of { happy_var_2 -> 
	case happyOut33 happy_x_4 of { happy_var_4 -> 
	case happyOut40 happy_x_6 of { happy_var_6 -> 
	happyIn35
		 (BNFC.AbsStretch.SLetType happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_48 = happySpecReduce_1  8# happyReduction_48
happyReduction_48 happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	happyIn35
		 (BNFC.AbsStretch.SBlockExp happy_var_1
	)}

happyReduce_49 = happySpecReduce_2  8# happyReduction_49
happyReduction_49 happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn35
		 (BNFC.AbsStretch.SExp happy_var_1
	)}

happyReduce_50 = happySpecReduce_2  8# happyReduction_50
happyReduction_50 happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	happyIn35
		 (happy_var_1
	)}

happyReduce_51 = happySpecReduce_0  9# happyReduction_51
happyReduction_51  =  happyIn36
		 ([]
	)

happyReduce_52 = happySpecReduce_2  9# happyReduction_52
happyReduction_52 happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_2 of { happy_var_2 -> 
	happyIn36
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_53 = happySpecReduce_3  10# happyReduction_53
happyReduction_53 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_2 of { happy_var_2 -> 
	happyIn37
		 (BNFC.AbsStretch.Block1 (reverse happy_var_2)
	)}

happyReduce_54 = happyReduce 4# 10# happyReduction_54
happyReduction_54 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut36 happy_x_2 of { happy_var_2 -> 
	case happyOut40 happy_x_3 of { happy_var_3 -> 
	happyIn37
		 (BNFC.AbsStretch.Block2 (reverse happy_var_2)happy_var_3
	) `HappyStk` happyRest}}

happyReduce_55 = happySpecReduce_1  11# happyReduction_55
happyReduction_55 happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	happyIn38
		 (BNFC.AbsStretch.EBlock happy_var_1
	)}

happyReduce_56 = happySpecReduce_3  11# happyReduction_56
happyReduction_56 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_2 of { happy_var_2 -> 
	case happyOut37 happy_x_3 of { happy_var_3 -> 
	happyIn38
		 (BNFC.AbsStretch.EIf happy_var_2 happy_var_3
	)}}

happyReduce_57 = happyReduce 5# 11# happyReduction_57
happyReduction_57 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut40 happy_x_2 of { happy_var_2 -> 
	case happyOut37 happy_x_3 of { happy_var_3 -> 
	case happyOut37 happy_x_5 of { happy_var_5 -> 
	happyIn38
		 (BNFC.AbsStretch.EIfElse happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_58 = happySpecReduce_3  11# happyReduction_58
happyReduction_58 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_2 of { happy_var_2 -> 
	case happyOut37 happy_x_3 of { happy_var_3 -> 
	happyIn38
		 (BNFC.AbsStretch.EWhile happy_var_2 happy_var_3
	)}}

happyReduce_59 = happySpecReduce_0  12# happyReduction_59
happyReduction_59  =  happyIn39
		 ([]
	)

happyReduce_60 = happySpecReduce_2  12# happyReduction_60
happyReduction_60 happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut38 happy_x_2 of { happy_var_2 -> 
	happyIn39
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_61 = happySpecReduce_3  13# happyReduction_61
happyReduction_61 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	case happyOut40 happy_x_3 of { happy_var_3 -> 
	happyIn40
		 (BNFC.AbsStretch.EAssign happy_var_1 happy_var_3
	)}}

happyReduce_62 = happySpecReduce_1  13# happyReduction_62
happyReduction_62 happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	happyIn40
		 (happy_var_1
	)}

happyReduce_63 = happySpecReduce_3  14# happyReduction_63
happyReduction_63 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	case happyOut42 happy_x_3 of { happy_var_3 -> 
	happyIn41
		 (BNFC.AbsStretch.EOr happy_var_1 happy_var_3
	)}}

happyReduce_64 = happySpecReduce_1  14# happyReduction_64
happyReduction_64 happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	happyIn41
		 (happy_var_1
	)}

happyReduce_65 = happySpecReduce_3  15# happyReduction_65
happyReduction_65 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn42
		 (BNFC.AbsStretch.EAnd happy_var_1 happy_var_3
	)}}

happyReduce_66 = happySpecReduce_1  15# happyReduction_66
happyReduction_66 happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	happyIn42
		 (happy_var_1
	)}

happyReduce_67 = happySpecReduce_3  16# happyReduction_67
happyReduction_67 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn43
		 (BNFC.AbsStretch.EEq happy_var_1 happy_var_3
	)}}

happyReduce_68 = happySpecReduce_3  16# happyReduction_68
happyReduction_68 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn43
		 (BNFC.AbsStretch.ENEq happy_var_1 happy_var_3
	)}}

happyReduce_69 = happySpecReduce_3  16# happyReduction_69
happyReduction_69 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn43
		 (BNFC.AbsStretch.ELess happy_var_1 happy_var_3
	)}}

happyReduce_70 = happySpecReduce_3  16# happyReduction_70
happyReduction_70 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn43
		 (BNFC.AbsStretch.ELEq happy_var_1 happy_var_3
	)}}

happyReduce_71 = happySpecReduce_3  16# happyReduction_71
happyReduction_71 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn43
		 (BNFC.AbsStretch.EGreat happy_var_1 happy_var_3
	)}}

happyReduce_72 = happySpecReduce_3  16# happyReduction_72
happyReduction_72 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn43
		 (BNFC.AbsStretch.EGEq happy_var_1 happy_var_3
	)}}

happyReduce_73 = happySpecReduce_1  16# happyReduction_73
happyReduction_73 happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	happyIn43
		 (happy_var_1
	)}

happyReduce_74 = happySpecReduce_3  17# happyReduction_74
happyReduction_74 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (BNFC.AbsStretch.EPlus happy_var_1 happy_var_3
	)}}

happyReduce_75 = happySpecReduce_3  17# happyReduction_75
happyReduction_75 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (BNFC.AbsStretch.EMinus happy_var_1 happy_var_3
	)}}

happyReduce_76 = happySpecReduce_1  17# happyReduction_76
happyReduction_76 happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	happyIn44
		 (happy_var_1
	)}

happyReduce_77 = happySpecReduce_3  18# happyReduction_77
happyReduction_77 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	case happyOut46 happy_x_3 of { happy_var_3 -> 
	happyIn45
		 (BNFC.AbsStretch.EMul happy_var_1 happy_var_3
	)}}

happyReduce_78 = happySpecReduce_3  18# happyReduction_78
happyReduction_78 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	case happyOut46 happy_x_3 of { happy_var_3 -> 
	happyIn45
		 (BNFC.AbsStretch.EDiv happy_var_1 happy_var_3
	)}}

happyReduce_79 = happySpecReduce_1  18# happyReduction_79
happyReduction_79 happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	happyIn45
		 (happy_var_1
	)}

happyReduce_80 = happySpecReduce_2  19# happyReduction_80
happyReduction_80 happy_x_2
	happy_x_1
	 =  case happyOut47 happy_x_2 of { happy_var_2 -> 
	happyIn46
		 (BNFC.AbsStretch.ENot happy_var_2
	)}

happyReduce_81 = happySpecReduce_2  19# happyReduction_81
happyReduction_81 happy_x_2
	happy_x_1
	 =  case happyOut47 happy_x_2 of { happy_var_2 -> 
	happyIn46
		 (BNFC.AbsStretch.ENeg happy_var_2
	)}

happyReduce_82 = happySpecReduce_1  19# happyReduction_82
happyReduction_82 happy_x_1
	 =  case happyOut47 happy_x_1 of { happy_var_1 -> 
	happyIn46
		 (happy_var_1
	)}

happyReduce_83 = happySpecReduce_1  20# happyReduction_83
happyReduction_83 happy_x_1
	 =  case happyOut32 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (BNFC.AbsStretch.ELit happy_var_1
	)}

happyReduce_84 = happySpecReduce_1  20# happyReduction_84
happyReduction_84 happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (BNFC.AbsStretch.EIdent happy_var_1
	)}

happyReduce_85 = happyReduce 4# 20# happyReduction_85
happyReduction_85 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut29 happy_x_1 of { happy_var_1 -> 
	case happyOut51 happy_x_3 of { happy_var_3 -> 
	happyIn47
		 (BNFC.AbsStretch.EStruct happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_86 = happyReduce 4# 20# happyReduction_86
happyReduction_86 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut47 happy_x_1 of { happy_var_1 -> 
	case happyOut48 happy_x_3 of { happy_var_3 -> 
	happyIn47
		 (BNFC.AbsStretch.ECall happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_87 = happyReduce 4# 20# happyReduction_87
happyReduction_87 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut40 happy_x_3 of { happy_var_3 -> 
	happyIn47
		 (BNFC.AbsStretch.EPrint happy_var_3
	) `HappyStk` happyRest}

happyReduce_88 = happySpecReduce_3  20# happyReduction_88
happyReduction_88 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut47 happy_x_1 of { happy_var_1 -> 
	case happyOut29 happy_x_3 of { happy_var_3 -> 
	happyIn47
		 (BNFC.AbsStretch.EField happy_var_1 happy_var_3
	)}}

happyReduce_89 = happySpecReduce_1  20# happyReduction_89
happyReduction_89 happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (BNFC.AbsStretch.EBlockExp happy_var_1
	)}

happyReduce_90 = happySpecReduce_1  20# happyReduction_90
happyReduction_90 happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (BNFC.AbsStretch.EAnonFun happy_var_1
	)}

happyReduce_91 = happySpecReduce_3  20# happyReduction_91
happyReduction_91 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_2 of { happy_var_2 -> 
	happyIn47
		 (happy_var_2
	)}

happyReduce_92 = happySpecReduce_0  21# happyReduction_92
happyReduction_92  =  happyIn48
		 ([]
	)

happyReduce_93 = happySpecReduce_1  21# happyReduction_93
happyReduction_93 happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn48
		 ((:[]) happy_var_1
	)}

happyReduce_94 = happySpecReduce_3  21# happyReduction_94
happyReduction_94 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	case happyOut48 happy_x_3 of { happy_var_3 -> 
	happyIn48
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_95 = happySpecReduce_2  22# happyReduction_95
happyReduction_95 happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_2 of { happy_var_2 -> 
	happyIn49
		 (BNFC.AbsStretch.AnonEmpty happy_var_2
	)}

happyReduce_96 = happyReduce 4# 22# happyReduction_96
happyReduction_96 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut53 happy_x_2 of { happy_var_2 -> 
	case happyOut37 happy_x_4 of { happy_var_4 -> 
	happyIn49
		 (BNFC.AbsStretch.AnonArgs happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_97 = happySpecReduce_3  23# happyReduction_97
happyReduction_97 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	case happyOut40 happy_x_3 of { happy_var_3 -> 
	happyIn50
		 (BNFC.AbsStretch.MemberExp happy_var_1 happy_var_3
	)}}

happyReduce_98 = happySpecReduce_0  24# happyReduction_98
happyReduction_98  =  happyIn51
		 ([]
	)

happyReduce_99 = happySpecReduce_1  24# happyReduction_99
happyReduction_99 happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	happyIn51
		 ((:[]) happy_var_1
	)}

happyReduce_100 = happySpecReduce_3  24# happyReduction_100
happyReduction_100 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	case happyOut51 happy_x_3 of { happy_var_3 -> 
	happyIn51
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_101 = happySpecReduce_3  25# happyReduction_101
happyReduction_101 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_3 of { happy_var_3 -> 
	happyIn52
		 (BNFC.AbsStretch.TypedIdent happy_var_1 happy_var_3
	)}}

happyReduce_102 = happySpecReduce_0  26# happyReduction_102
happyReduction_102  =  happyIn53
		 ([]
	)

happyReduce_103 = happySpecReduce_1  26# happyReduction_103
happyReduction_103 happy_x_1
	 =  case happyOut52 happy_x_1 of { happy_var_1 -> 
	happyIn53
		 ((:[]) happy_var_1
	)}

happyReduce_104 = happySpecReduce_3  26# happyReduction_104
happyReduction_104 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_3 of { happy_var_3 -> 
	happyIn53
		 ((:) happy_var_1 happy_var_3
	)}}

happyNewToken action sts stk [] =
	happyDoAction 41# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TS _ 31) -> cont 31#;
	PT _ (TS _ 32) -> cont 32#;
	PT _ (TS _ 33) -> cont 33#;
	PT _ (TS _ 34) -> cont 34#;
	PT _ (TS _ 35) -> cont 35#;
	PT _ (TS _ 36) -> cont 36#;
	PT _ (TS _ 37) -> cont 37#;
	PT _ (TI happy_dollar_dollar) -> cont 38#;
	PT _ (TL happy_dollar_dollar) -> cont 39#;
	PT _ (TV happy_dollar_dollar) -> cont 40#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 41# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => ([(Token)], [String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pProgram tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut30 x))

pBoolean tks = happySomeParser where
 happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (happyOut31 x))

pLiteral tks = happySomeParser where
 happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (happyOut32 x))

pType tks = happySomeParser where
 happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (happyOut33 x))

pListType tks = happySomeParser where
 happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (happyOut34 x))

pStm tks = happySomeParser where
 happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (happyOut35 x))

pListStm tks = happySomeParser where
 happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (happyOut36 x))

pBlock tks = happySomeParser where
 happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (happyOut37 x))

pBlockExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (happyOut38 x))

pListBlockExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (happyOut39 x))

pExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (happyOut40 x))

pExp1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (happyOut41 x))

pExp2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 12# tks) (\x -> happyReturn (happyOut42 x))

pExp3 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 13# tks) (\x -> happyReturn (happyOut43 x))

pExp4 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 14# tks) (\x -> happyReturn (happyOut44 x))

pExp5 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 15# tks) (\x -> happyReturn (happyOut45 x))

pExp6 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 16# tks) (\x -> happyReturn (happyOut46 x))

pExp7 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 17# tks) (\x -> happyReturn (happyOut47 x))

pListExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse 18# tks) (\x -> happyReturn (happyOut48 x))

pAnonFunc tks = happySomeParser where
 happySomeParser = happyThen (happyParse 19# tks) (\x -> happyReturn (happyOut49 x))

pMemberInit tks = happySomeParser where
 happySomeParser = happyThen (happyParse 20# tks) (\x -> happyReturn (happyOut50 x))

pListMemberInit tks = happySomeParser where
 happySomeParser = happyThen (happyParse 21# tks) (\x -> happyReturn (happyOut51 x))

pFormalArg tks = happySomeParser where
 happySomeParser = happyThen (happyParse 22# tks) (\x -> happyReturn (happyOut52 x))

pListFormalArg tks = happySomeParser where
 happySomeParser = happyThen (happyParse 23# tks) (\x -> happyReturn (happyOut53 x))

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    t:_ -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 11 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "/tmp/ghcab1e_0/ghc_2.h" #-}




















































































































































































































































































{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}


          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}


                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+#  i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 180 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+#  nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+#  nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
