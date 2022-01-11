
package body ARFF is

function Decode (File_Data : String; Encode_Nominal : Boolean := False;
                  Return_Type : ARFF_Return_Type := Arff_Dense)
                  return JSON_Value is
     ARFF_Data : JSON_Value;
   begin
      return ARFF_Data;

   end Decode;

   --  -------------------------------------------------------------------------

   function Load (File_Data : String; Encode_Nominal : Boolean := False;
                  Return_Type : ARFF_Return_Type := Arff_Dense)
                  return JSON_Value is
     ARFF_Data : JSON_Value := Decode (File_Data, Encode_Nominal, Return_Type);
   begin
      return ARFF_Data;

   end Load;

   --  -------------------------------------------------------------------------

end ARFF;
