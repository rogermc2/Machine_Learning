--  Based on scikit-learn/sklearn/mixture/_base.py

with Base;

package body Base_Mix is

   --  -------------------------------------------------------------------------
   --  L356
   function Score (X    : IL_Types.Float_List_2D;
                   Y    : IL_Types.Integer_List) return Float is
   begin
      pragma Unreferenced (Y);
      return Score_Samples (X);

   end Score;

   --  -------------------------------------------------------------------------
    --  L337
   function Score_Samples (X : IL_Types.Float_List_2D) return Float is
   begin

      return Base.Estimate_Weighted_Log_Prob (X);

   end Score_Samples;

   --  -------------------------------------------------------------------------

end Base_Mix;
