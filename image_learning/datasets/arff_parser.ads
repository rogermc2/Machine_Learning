--  Based on scikit-learn/sklearn/datasets _arff_parser.py

with AR_Types;
with IL_Types;

package ARFF_Parser is

   procedure Arff_Parser
     (ARFF_Container : AR_Types.ARFF_Record;
      Features_Dict  : AR_Types.Attribute_Dictionary_Map;
      Data_Columns   : IL_Types.String_List;
      Target_Columns : IL_Types.String_List;
      Col_Slice_X    : IL_Types.Integer_DL_List;
      Col_Slice_Y    : IL_Types.Integer_DL_List;
      X              : out IL_Types.Float_List_2D;
      Y              : out IL_Types.Integer_List);
   procedure Convert_Arff_Data
     (ARFF_Container : AR_Types.ARFF_Record;
      Col_Slice_X    : IL_Types.Integer_DL_List;
      Col_Slice_Y    : IL_Types.Integer_DL_List;
      X              : out IL_Types.Float_List_2D;
      Y              : out IL_Types.Integer_List);
   function Parse_Nominal_Data
     (Arff_Data       : AR_Types.ARFF_Record;
      Include_Columns : IL_Types.String_List) return AR_Types.Nominal_Data_List;

   --     function Convert_Arff_Data_Dataframe
   --       (ARFF_Container : ARFF.Arff_Container_Type; Features : JSON_Value)
   --        return JSON_Value;

end ARFF_Parser;
