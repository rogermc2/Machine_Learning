
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Types;

with AR_Types;

package Openml_Ada is

   type As_Frame_State is (As_Frame_False, As_Frame_True, As_Frame_Auto);
   --     subtype Qualities_Map is JSON_Array;

   type Bunch_Data is record
      As_Frame      : As_Frame_State := As_Frame_False;
      Categories    : AR_Types.Nominal_Data_List;
      Feature_Names : ML_Types.String_List;
      Target_Names  : ML_Types.String_List;
   end record;

   type Shape_Data is record
      Num_Samples     : Natural := 0;
      Features_Length : Natural := 0;
   end record;

   procedure Download_Data_To_Bunch
     (ARFF_Container               : AR_Types.ARFF_Record;
      Features_List                : AR_Types.Attribute_List;
      Data_Columns, Target_Columns : ML_Types.String_List;
      X                            : out ML_Types.Value_Data_Lists_2D;
      Y                            : out ML_Types.Value_Data_Lists_2D;
      Bunch                        : out Bunch_Data;
      X_Y_Only                     : Boolean := False;
      --        Sparse                     : Boolean;
      As_Frame                     : As_Frame_State := As_Frame_False);
   procedure Fetch_Openml (Dataset_File_Name : String;
                           Save_File_Name    : String;
                           Target_Column     : ML_Types.String_List;
                           X, Y              : out ML_Types.Value_Data_Lists_2D;
                           X_Indices         : out ML_Types.Integer_List;
                           Y_Indices         : out ML_Types.Integer_List;
                           Bunch             : out Bunch_Data;
                           As_Frame          : in out As_Frame_State;
                           Return_X_Y        : Boolean := False);
   function Valid_Data_Column_Names
     (Features_List  : AR_Types.Attribute_List;
      Target_Columns : ML_Types.String_List) return ML_Types.String_List;

end Openml_Ada;
