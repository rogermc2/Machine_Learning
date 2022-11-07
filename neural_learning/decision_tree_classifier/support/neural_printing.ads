
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Encode_Utils;
with Stochastic_Optimizers;

package Neural_Printing is

   Print_Error : Exception;

   procedure Print_Integer_Set (Name   : String;
                                theSet : Encode_Utils.Int_Sets.Set);

   procedure Print_Parameters
     (Name       : String; Params : Stochastic_Optimizers.Parameters_Record;
      Rows_Start : Positive := 1; Rows_Last : Positive := 10);

   procedure Print_Unbounded_Set (Name   : String;
                                  theSet : Encode_Utils.UB_String_Sets.Set);
end ;
