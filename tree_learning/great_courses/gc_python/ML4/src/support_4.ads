
with Base_Decision_Tree;
with ML_Types; use ML_Types;
with Openml_Ada;
with Tree;

package Support_4 is

   function Get_State
     (Dataset_Name     : String; Return_X_Y : Boolean;
      X, Y             : out Value_Data_Lists_2D;
      X_Indices        : out ML_Types.Integer_List;
      Y_Indices        : out ML_Types.Integer_List;
      Test_X, Test_Y,
      Train_X, Train_Y : out Value_Data_Lists_2D;
      Bunch            : out Openml_Ada.Bunch_Data) return Boolean;
   function Get_Tree (Dataset_Name : String; theTree : out Tree.Tree_Class)
                      return Boolean;
   procedure Save_State
     (Dataset_Name               : String;
      Save_Test_X, Save_Test_Y,
      Save_Train_X, Save_Train_Y : Value_Data_Lists_2D;
      Save_Bunch                 : Openml_Ada.Bunch_Data);
   procedure Save_Tree
     (Dataset_Name : String; Classifier : Base_Decision_Tree.Classifier);

end Support_4;
