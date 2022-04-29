--  Based on scikit-learn/sklearn/datasets _arff_parser.py

with AR_Types;
with NL_Types;

package ARFF_Parser is

   procedure Arff_Parser
     (ARFF_Container : AR_Types.ARFF_Record;
      Target_Columns : NL_Types.String_List;
      Col_Slice_X    : NL_Types.Integer_DL_List;
      Col_Slice_Y    : NL_Types.Integer_DL_List;
      X              : out NL_Types.Float_List_2D;
      Y              : out NL_Types.Integer_List);
   function Parse_Nominal_Data
     (Arff_Data       : AR_Types.ARFF_Record;
      Include_Columns : NL_Types.String_List) return AR_Types.Nominal_Data_List;

   --     function Convert_Arff_Data_Dataframe
   --       (ARFF_Container : ARFF.Arff_Container_Type; Features : JSON_Value)
   --        return JSON_Value;

end ARFF_Parser;
