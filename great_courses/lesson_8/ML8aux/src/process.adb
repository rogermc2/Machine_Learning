
with Ada.Text_IO; use Ada.Text_IO;

with Python_API;
with Python_CLF;
with Support_8Aux; use Support_8Aux;

package body Process is

   procedure Process_Decision_Tree
     (Classifier            : Python.Module;
      Data, Test_Data       : Real_Float_Matrix;
      Train_Labs, Test_Labs : Boolean_Array; Max_Leaf_Nodes : Positive) is
      Train_Pred    : Boolean_Array (Data'Range);
      Test_Pred     : Boolean_Array (Test_Data'Range);
      Decision_Tree : constant Python_API.PyObject :=
                        Python.Call (Classifier, "init_DecisionTreeClassifier",
                                     Max_Leaf_Nodes);
   begin
      Python_CLF.Call (Classifier, "fit", Decision_Tree, Data, Train_Labs);

      Train_Pred := Python_CLF.Call (Classifier, "predict",
                                     Decision_Tree, Data);
      Test_Pred := Python_CLF.Call (Classifier, "predict",
                                    Decision_Tree, Test_Data);
      declare
         Predictions : constant Unbounded_String_Array :=
                         Get_Predictions (Test_Pred, Test_Labs);
      begin
         Put_Line ("Decision Tree Train accuracy: " &
                     Float'Image (Accuracy (Train_Pred, Train_Labs)));
         Put_Line ("Decision Tree Test accuracy: " &
                     Float'Image (Accuracy (Test_Pred, Test_Labs)));
         Python.Call (Classifier, "plot_predictions", "Decision Tree Model", Test_Data, Predictions);
      end;

      Python_CLF.Call (Classifier, "print_tree", Decision_Tree);

      Python_API.Py_DecRef (Decision_Tree);

   end  Process_Decision_Tree;

   --  ------------------------------------------------------------------------

   procedure Process_Neighbours
     (Classifier            : Python.Module;
      Data, Test_Data       : Real_Float_Matrix;
      Train_Labs, Test_Labs : Boolean_Array; Num_Neighbours: Positive) is
      Neighbours       : constant Python_API.PyObject :=
                           Python.Call (Classifier, "init_NeighborsClassifier",
                                        Num_Neighbours);
      Train_Pred       : Boolean_Array (Data'Range);
      Test_Pred        : Boolean_Array (Test_Data'Range);
   begin
      Python_CLF.Call (Classifier, "fit", Neighbours, Data, Train_Labs);
      Train_Pred := Python_CLF.Call (Classifier, "predict", Neighbours, Data);
      Test_Pred := Python_CLF.Call (Classifier, "predict", Neighbours,
                                    Test_Data);
      declare
         Predictions : constant Unbounded_String_Array :=
                         Get_Predictions (Test_Pred, Test_Labs);
      begin
         Put_Line ("Neighbours Train accuracy: " &
                     Float'Image (Accuracy (Train_Pred, Train_Labs)));
         Put_Line ("Neighbours Test accuracy: " &
                     Float'Image (Accuracy (Test_Pred, Test_Labs)));
         Python.Call (Classifier, "plot_predictions", "K-Nearest Neighbours",
                      Test_Data, Predictions);
      end;

      Python_API.Py_DecRef (Neighbours);

   end Process_Neighbours;

   --  ------------------------------------------------------------------------

end Process;
