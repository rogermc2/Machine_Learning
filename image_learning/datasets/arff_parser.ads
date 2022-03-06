--  Based on scikit-learn/sklearn/datasets _arff_parser.py

with AR_Types;
with IL_Types;

package ARFF_Parser is

   function Parse_Nominal_Data
     (Arff_Data       : AR_Types.ARFF_Record;
      Include_Columns : IL_Types.String_List) return AR_Types.Nominal_Data_List;
   function Split_Columns
     (Arff_Data       : AR_Types.AR_Real_List_2D;
      Include_Columns : IL_Types.Integer_DL_List) return IL_Types.Float_List_2D;
   function Split_Columns
     (Arff_Target    : AR_Types.AR_Integer_List;
      Include_Values : IL_Types.Integer_DL_List) return IL_Types.Integer_List;

   --     function Convert_Arff_Data_Dataframe
   --       (ARFF_Container : ARFF.Arff_Container_Type; Features : JSON_Value)
   --        return JSON_Value;

end ARFF_Parser;
