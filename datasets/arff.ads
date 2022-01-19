
with GNATCOLL.JSON; use GNATCOLL.JSON;

package ARFF is

   type ARFF_Return_Type is (Arff_Dense, Arff_Coo, Arff_Lod,
                             Arff_Dense_Gen, Arff_Lod_Gen);

   ARFF_Error : Exception;

   function Load (File_Data : String; Encode_Nominal : Boolean := False;
                  Return_Type : ARFF_Return_Type := Arff_Dense) return JSON_Value;

end ARFF;
