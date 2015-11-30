module Datapath where

{- This module defines a basic datapath that supports a subset of the
Sigma16 architecture.  The datapath contains a ripple carry adder
instead of a full ALU.  It can be simulated using RunDatapath1. -}

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational
import HDL.Hydra.Circuits.Register

import ControlSignals
import ALU
import Multiply


------------------------------------------------------------------------
--			       Datapath
------------------------------------------------------------------------

{- The datapath contains the registers, computational systems, and
interconnections.  It has two inputs: a set of control signals
provided by the control unit, and a data word from the either the
memory system or the DMA input controller. -}

datapath ctlsigs memdat = (ma,md,cond,a,b,ir,pc,ad,ovfl,r,x,y,p1,ready,prod,rx,ry,multiply_s)
  where

-- Size parameters
      n = 16    -- word size
      k =  4    -- the register file contains 2^k registers
      z =  8

-- Registers
      (a,b) = regfile n k (ctl_rf_ld ctlsigs) ir_dd rf_sa rf_sb p1
      ir = reg n (ctl_ir_ld ctlsigs) memdat
      pc = reg n (ctl_pc_ld ctlsigs) q
      ad = reg n (ctl_ad_ld ctlsigs) (mux1w (ctl_ad_alu ctlsigs) memdat r)

-- The ALU
      (ovfl,r) = alu n (ctl_alu_a ctlsigs, ctl_alu_b ctlsigs,
                        ctl_alu_c ctlsigs, ctl_alu_d ctlsigs)
                       x y

-- The Multiply
      (ready,prod,rx,ry,multiply_s) = multiply n (ctl_mul_start ctlsigs) x y

-- Internal processor signals
      x = mux1w (ctl_x_pc ctlsigs) a pc             -- alu input 1
      y = mux1w (ctl_y_ad ctlsigs) b ad             -- alu input 2
      rf_sa = mux1w (ctl_rf_sd ctlsigs) ir_sa ir_d  -- a = reg[rf_sa]
      rf_sb = ir_sb                                 -- b = reg[rf_sb]
      p  = mux1w (ctl_rf_pc ctlsigs)                -- regfile data input
             (mux1w (ctl_rf_alu ctlsigs) memdat r)
             pc
      p1 = mux1w (ctl_rf_mul_ld ctlsigs) p (field prod 16 16)
      q = mux1w (ctl_pc_ad ctlsigs) r ad        -- input to pc
      ma = mux1w (ctl_ma_pc ctlsigs) ad pc      -- memory address
      md = a                                    -- memory data
      cond = orw a                              -- boolean for conditionals

-- Decide destination
      ir_dd = mux1w (ctl_rf_ds ctlsigs) ir_d ir_sa

-- Instruction fields
      ir_op = field ir  0 4           -- instruction opcode
      ir_d  = field ir  4 4           -- instruction destination register
      ir_sa = field ir  8 4           -- instruction source a register
      ir_sb = field ir 12 4           -- instruction source b register
