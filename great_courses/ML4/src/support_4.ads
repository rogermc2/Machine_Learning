
with Base_Decision_Tree;
with ML_Types; use ML_Types;
with Openml_Ada;

package Support_4 is

   procedure Get_State
     (Dataset_Name                 : String;
      Saved_Test_X, Saved_Test_Y,
      Saved_Train_X, Saved_Train_Y : out Value_Data_Lists_2D;
      Saved_Bunch                  : out Openml_Ada.Bunch_Data);
   procedure Save_State
     (Dataset_Name               : String;
      Save_Test_X, Save_Test_Y,
      Save_Train_X, Save_Train_Y : Value_Data_Lists_2D;
      Save_Bunch                 : Openml_Ada.Bunch_Data);
   procedure Save_Tree
     (Dataset_Name : String; Classifier : Base_Decision_Tree.Classifier);

end Support_4;
