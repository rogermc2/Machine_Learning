--  Based on scikit-learn/sklearn/datasets _arff_parser.py

with AR_Types;
with NL_Types; use NL_Types;

package ARFF_Parser is

   procedure Arff_Parser
     (ARFF_Container : AR_Types.ARFF_Record;
      Target_Columns : String_List;
      Col_Slice_X    : Integer_DL_List;
      Col_Slice_Y    : Integer_DL_List;
      X              : out Float_List_2D;
      Y              : out Integer_List);
   function Parse_Nominal_Data
     (Arff_Data       : AR_Types.ARFF_Record;
      Include_Columns : String_List) return AR_Types.Nominal_Data_List;

end ARFF_Parser;
