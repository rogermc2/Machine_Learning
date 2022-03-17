--  Based on scikit-learn/sklearn/mixture/_base.py

with Neural_Maths;

package body Base_Mix is

   --  -------------------------------------------------------------------------
    --  L498 _estimate_log_prob_resp computes the log-probabilities per
   --   component for each sample.
   function Estimate_Log_Prob (X : Float_List_2D) return Float_List_2D is
      Result : constant Float_List_2D := X;  --  dummy
   begin

      return Result;

   end Estimate_Log_Prob;

   --  -------------------------------------------------------------------------
   --  L513 _estimate_log_prob_resp computes the log-probabilities per
   --   component for each sample.
   procedure Estimate_Log_Prob_Resp
      (X : Float_List_2D; Log_Prob_Norm : out Float_List;
       Log_Responsibil : out Float_List_2D ) is
      Weighted_Log_Prob : Float_List_2D := Estimate_Weighted_Log_Prob (X);
      Result : Float;
   begin
      Log_Prob_Norm := Neural_Maths.Log_Sum_Exponent (Weighted_Log_Prob);

   end Estimate_Log_Prob_Resp;

   --  -------------------------------------------------------------------------

    --  L474  BaseMixture.Estimate_Log_Prob
   function Estimate_Weighted_Log_Prob (X : Float_List_2D)
                                        return Float_List_2D is
   begin

      return Estimate_Log_Prob (X); -- + Estimate_Log_Weights;

   end Estimate_Weighted_Log_Prob;

   --  -------------------------------------------------------------------------
    --  L356
   function Score (X    : IL_Types.Float_List_2D;
                   Y    : IL_Types.Integer_List) return Float is
   begin
      pragma Unreferenced (Y);
      return Neural_Maths.Mean (Score_Samples (X));

   end Score;

   --  -------------------------------------------------------------------------
   --  L337
   function Score_Samples (X : IL_Types.Float_List_2D) return Float_List is
   begin

      return Estimate_Weighted_Log_Prob (X);

   end Score_Samples;

   --  -------------------------------------------------------------------------

end Base_Mix;
