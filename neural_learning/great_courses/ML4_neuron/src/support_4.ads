
with Base_Decision_Tree;
with NL_Types; use NL_Types;
with Openml_Ada;

package Support_4 is

   function Get_State
     (Dataset_Name : String;
      --        X_Indices    : out Integer_List;
      --        Y_Indices    : out Integer_List;
      Train_X      : out Float_List_2D;
      Train_Y      : out Integer_List;
      Test_X       : out Float_List_2D;
      Test_Y       : out Integer_List;
      Bunch        : out Openml_Ada.Bunch_Data) return Boolean;
   function Get_Classifier (Dataset_Name : String;
                            Classifier   : out Base_Decision_Tree.Classifier)
                            return Boolean;
   procedure Save_State
     (Dataset_Name : String;
      Train_X      : Float_List_2D;
      Train_Y      : Integer_List;
      Test_X       : Float_List_2D;
      Test_Y       : Integer_List;
      Save_Bunch   : Openml_Ada.Bunch_Data);
   procedure Save_Classifier
     (Dataset_Name : String; Classifier : Base_Decision_Tree.Classifier);

end Support_4;
