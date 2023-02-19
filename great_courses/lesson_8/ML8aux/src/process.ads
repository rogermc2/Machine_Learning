
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;

package Process is

   procedure Process_Decision_Tree
     (Classifier            : Python.Module;
      Data, Test_Data       : Real_Float_Matrix;
      Train_Labs, Test_Labs : Boolean_Array; Max_Leaf_Nodes : Positive);
   procedure Process_Neighbours
     (Classifier            : Python.Module;
      Data, Test_Data       : Real_Float_Matrix;
      Train_Labs, Test_Labs : Boolean_Array; Num_Neighbours: Positive);

end Process;
