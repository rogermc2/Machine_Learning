--  Based on scikit-learn/sklearn/mixture/_base.py

with Neural_Maths;

package body Base_Mix is

   --  -------------------------------------------------------------------------
    --  L498 _estimate_log_prob_resp computes the log-probabilities per
   --   component for each sample.
   function Estimate_Log_Prob (X : Float_Matrix) return Float_Matrix is
      Result : constant Float_Matrix := X;  --  dummy
   begin

      return Result;

   end Estimate_Log_Prob;

   --  -------------------------------------------------------------------------
   --  L513 _estimate_log_prob_resp computes the log-probabilities per
   --       component for each sample.
   --  Log_Prob_Norm for num_samples
   --  Log_Responsibil for num_samples x num_components
   procedure Estimate_Log_Prob_Resp
      (X : Float_Matrix; Log_Prob_Norm : out Float_Array;
       Log_Responsibil : out Float_Matrix) is
      Weighted_Log_Prob : constant Float_Matrix :=
                              Estimate_Weighted_Log_Prob (X);
   begin
      Log_Prob_Norm := Neural_Maths.Log_Sum_Exponent (Weighted_Log_Prob);
      declare
         Log_Prob_Norm_M  : Float_Matrix (1 .. 1, 1 .. Log_Prob_Norm'Length);
      begin
         for index in Log_Prob_Norm'First .. Log_Prob_Norm'Last loop
            Log_Prob_Norm_M (1, index) := Log_Prob_Norm (index);
         end loop;
         Log_Responsibil := Weighted_Log_Prob - Log_Prob_Norm_M;
      end;

   end Estimate_Log_Prob_Resp;

   --  -------------------------------------------------------------------------

    --  L474  BaseMixture.Estimate_Log_Prob
   function Estimate_Weighted_Log_Prob (X : Float_Matrix)
                                        return Float_Matrix is
   begin

      return Estimate_Log_Prob (X); -- + Estimate_Log_Weights;

   end Estimate_Weighted_Log_Prob;

   --  -------------------------------------------------------------------------
   --  L356 Score returns the Log-likelihood of X (Float) under the Gaussian
   --  mixture model.
   function Score (X : Float_Matrix) return Float is
   begin
      return Neural_Maths.Mean (Score_Samples (X));

   end Score;

   --  -------------------------------------------------------------------------
   --  L337 returns Float_List num_samples
   function Score_Samples (X : Float_Matrix) return Float_Array is
        use Neural_Maths;
   begin

      return Log_Sum_Exponent (Estimate_Weighted_Log_Prob (X));

   end Score_Samples;

   --  -------------------------------------------------------------------------

end Base_Mix;
