
with Base_Decision_Tree;
with IL_Types; use IL_Types;
with Openml_Ada;
with Tree;

package Support_4 is

   function Get_State
     (Dataset_Name : String; Return_X_Y : Boolean;
      X            : out Float_List_2D;
      Y            : out Integer_List;
--        X_Indices    : out Integer_List;
--        Y_Indices    : out Integer_List;
      Train_X      : out Float_List_2D;
      Train_Y      : out Integer_List;
      Test_X       : out Float_List_2D;
      Test_Y       : out Integer_List;
      Bunch        : out Openml_Ada.Bunch_Data) return Boolean;
   function Get_Tree (Dataset_Name : String; theTree : out Tree.Tree_Class)
                      return Boolean;
   procedure Save_State
     (Dataset_Name : String;
      Train_X      : Float_List_2D;
      Train_Y      : Integer_List;
      Test_X       : Float_List_2D;
      Test_Y       : Integer_List;
      Save_Bunch   : Openml_Ada.Bunch_Data);
   procedure Save_Tree
     (Dataset_Name : String; Classifier : Base_Decision_Tree.Classifier);

end Support_4;
