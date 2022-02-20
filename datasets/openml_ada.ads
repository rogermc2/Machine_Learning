
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Types;

with Load_ARFF_Data;

package Openml_Ada is

    type As_Frame_State is (As_Frame_False, As_Frame_True, As_Frame_Auto);
    --     subtype Qualities_Map is JSON_Array;

    type Bunch_Data (Only_XY : Boolean := False) is record
        Data          : Load_ARFF_Data.ARFF_Data_List_2D;
        Target        : Load_ARFF_Data.ARFF_Data_List_2D;
        case Only_XY is
            when True => null;
            when False =>
                As_Frame      : As_Frame_State := As_Frame_False;
                Categories    : Load_ARFF_Data.Nominal_Data_List;
                Feature_Names : ML_Types.String_List;
                Target_Names  : ML_Types.String_List;
        end case;
    end record;

    type Shape_Data is record
        Num_Samples     : Natural := 0;
        Features_Length : Natural := 0;
    end record;

    function Download_Data_To_Bunch
      (ARFF_Container : Load_ARFF_Data.ARFF_Record;
       Features_List  : Load_ARFF_Data.Attribute_List;
       Data_Columns, Target_Columns : ML_Types.String_List;
       Return_X_Y     : Boolean := False;
       --        Sparse         : Boolean;
       As_Frame       : As_Frame_State := As_Frame_False)
      return Bunch_Data;
    function Fetch_Openml (Dataset_File_Name : String;
                           Target_Column     : ML_Types.String_List;
                           As_Frame          : in out As_Frame_State;
                           Return_X_Y        : Boolean := False)
                          return Bunch_Data;
    function Valid_Data_Column_Names
      (Features_List  : Load_ARFF_Data.Attribute_List;
       Target_Columns : ML_Types.String_List) return ML_Types.String_List;

end Openml_Ada;
