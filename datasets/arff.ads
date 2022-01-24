
with GNATCOLL.JSON; use GNATCOLL.JSON;

with ML_Types;

package ARFF is

   subtype Arff_Container_Type is JSON_Value;
   subtype Arff_Sparse_Data_Type is ML_Types.String_Multi_Vector;

   type ARFF_Return_Type is (Arff_Dense, Arff_Coo, Arff_Lod,
                             Arff_Dense_Gen, Arff_Lod_Gen);

   ARFF_Error : Exception;

   function Load (File_Data : String; Return_Type : ARFF_Return_Type := Arff_Dense)
                  return JSON_Value;

end ARFF;
