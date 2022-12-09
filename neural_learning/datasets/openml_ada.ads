
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with AR_Types;
with ML_Types; use ML_Types;
with NL_Types; use NL_Types;

package Openml_Ada is

    type As_Frame_State is (As_Frame_False, As_Frame_True, As_Frame_Auto);

   type Bunch_Data is record
      Data          : Float_List_2D;
      Target        : Integer_List;
      As_Frame      : As_Frame_State := As_Frame_False;
      Categories    : AR_Types.Nominal_Data_List;
      Feature_Names : String_Vector;
      Target_Names  : String_List;
   end record;

    type Shape_Data is record
        Num_Samples     : Natural := 0;
        Features_Length : Natural := 0;
    end record;

    procedure Download_Data_To_Bunch
      (ARFF_Container      : AR_Types.ARFF_Record;
       Features_List       : AR_Types.Attribute_List;
       Data_Column_Names   : String_Vector;
       Target_Column_Names : in out String_List;
       Bunch               : out Bunch_Data;
       --        Sparse       : Boolean;
       As_Frame            : As_Frame_State := As_Frame_False);
    procedure Fetch_Openml
      (Dataset_File_Name : String;
       Save_File_Name    : String;
       Target_Columns    : in out String_List;
       Bunch             : out Bunch_Data;
       As_Frame          : in out As_Frame_State);
    function Valid_Data_Column_Names
      (Features_List  : AR_Types.Attribute_List;
       Target_Columns : String_List) return String_Vector;

end Openml_Ada;
