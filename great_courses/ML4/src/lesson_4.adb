
with Ada.Containers;
with Ada.Directories;
with Ada.Assertions; use Ada.Assertions;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

with ML_Types;

with Base_Decision_Tree;
with Classifier_Types;
--  with Classifier_Utilities;
with Criterion;
with Data_Splitter;
with Decision_Tree_Classification;
--  with Graphviz_Exporter;
with Load_ARFF_Data;
with Openml_Ada;
with Printing;
with Tree;
with Weights;

procedure Lesson_4 is
   use Ada.Containers;
   use Ada.Directories;
   use ML_Types;
   use String_Package;
   use Load_ARFF_Data;
   use Decision_Tree_Classification;
   Routine_Name  : constant String := "Lesson_4 ";
   --     Dataset_File  : constant String := "../mnist_784.arff";
   --     Save_File     : constant String := "mnist_784.oml";
   --     State_File    : constant String := "mnist_784.sta";
   --     Return_X_Y    : constant Boolean := True;
   Dataset_File  : constant String := "../diabetes.arff";
   Save_File     : constant String := "diabetes.oml";
   State_File    : constant String := "diabetes.sta";
   Return_X_Y    : constant Boolean := False;
   Min_Split     : constant String := "2";
   As_Frame      : Openml_Ada.As_Frame_State := Openml_Ada.As_Frame_False;
   Bunch         : Openml_Ada.Bunch_Data;
   X             : Value_Data_Lists_2D;  --  rows of columns of values
   Y             : Value_Data_Lists_2D;
   Num_Samples   : Positive;
   Test_Size     : Positive;
   Train_Size    : Positive;
   Test_X        : Value_Data_Lists_2D;
   Test_Y        : Value_Data_Lists_2D;
   Train_X       : Value_Data_Lists_2D;
   Train_Y       : Value_Data_Lists_2D;
   aClassifier   : Base_Decision_Tree.Classifier
     (Tree.Integer_Type, Tree.Integer_Type, Tree.Integer_Type);
   No_Weights    : Weights.Weight_List :=
                     Classifier_Types.Float_Package.Empty_Vector;
   --     Correct       : Natural := 0;
   --     Exporter      : Graphviz_Exporter.DOT_Tree_Exporter;
   procedure Get_State
     (Saved_Test_X, Saved_Test_Y, Saved_Train_X, Saved_Train_Y :
      out Value_Data_Lists_2D;
      Saved_Bunch                                              :
      out Openml_Ada.Bunch_Data) is
      use Ada.Streams;
      use Stream_IO;
      --        Routine_Name : constant String := "Lesson_4.Get_State ";
      File_ID      : Stream_IO.File_Type;
      aStream      : Stream_Access;
   begin
      Open (File_ID, In_File, State_File);
      aStream := Stream (File_ID);
      Value_Data_Lists_2D'Read (aStream, Saved_Test_X);
      Value_Data_Lists_2D'Read (aStream, Saved_Test_Y);
      Value_Data_Lists_2D'Read (aStream, Saved_Train_X);
      Value_Data_Lists_2D'Read (aStream, Saved_Train_Y);
      Openml_Ada.Bunch_Data'Read (aStream, Saved_Bunch);
      Close (File_ID);
      pragma Unreferenced (File_ID);

   end Get_State;

   procedure Save_State
     (Save_Test_X, Save_Test_Y, Save_Train_X, Save_Train_Y : Value_Data_Lists_2D;
      Save_Bunch                                           : Openml_Ada.Bunch_Data) is
      use Ada.Streams;
      use Stream_IO;
      --        Routine_Name : constant String := "Lesson_4.Save_State ";
      File_ID      : Stream_IO.File_Type;
      aStream      : Stream_Access;
   begin
      Create (File_ID, Out_File, State_File);
      aStream := Stream (File_ID);
      Value_Data_Lists_2D'Write (aStream, Save_Test_X);
      Value_Data_Lists_2D'Write (aStream, Save_Test_Y);
      Value_Data_Lists_2D'Write (aStream, Save_Train_X);
      Value_Data_Lists_2D'Write (aStream, Save_Train_Y);
      Openml_Ada.Bunch_Data'Write (aStream, Save_Bunch);
      Close (File_ID);
      pragma Unreferenced (File_ID);
   end Save_State;

begin
   Put_Line (Routine_Name);

   if Exists (State_File) then
      Put_Line (Routine_Name & "restoring state");
      Get_State (Test_X, Test_Y, Train_X, Train_Y, Bunch);
      Put_Line (Routine_Name & "state restored");
   else
      Openml_Ada.Fetch_Openml (Dataset_File_Name => Dataset_File,
                               Save_File_Name    => Save_File,
                               Target_Column     => Empty_List,
                               X                 => X,
                               Y                 => Y,
                               Bunch             => Bunch,
                               As_Frame          => As_Frame,
                               Return_X_Y        => Return_X_Y);
      Num_Samples := Positive (X.Length);
      Test_Size := Num_Samples / 4;
      Train_Size := Num_Samples - Test_Size;

      Put_Line (Routine_Name & "Num_Samples" & Integer'Image (Num_Samples));
      Assert (X.Length > 0, Routine_Name & "X is empty.");
      Assert (Y.Length > 0, Routine_Name & "Y is empty.");

      Assert (Natural (Y.Length) = Num_Samples, Routine_Name &
                "Y length" & Count_Type'Image (Y.Length) &
                " is different to X length" & Natural'Image (Num_Samples));
      Printing.Print_Value_Data_List ("Features row 16", X.Element (16));

      Put_Line (Routine_Name & "permuting");
      X := Permute (X);
      Put_Line (Routine_Name & "X permuted");
      Y := Permute (Y);
      Put_Line (Routine_Name & "Y permuted");
      Printing.Print_Value_Data_List ("permuted features row 16",
                                      X.Element (16));

      Data_Splitter.Train_Test_Split (X, Y, Test_Size, Train_Size,
                                      Test_X, Test_Y, Train_X, Train_Y);
      --     ARFF_Printing.Print_Data (Routine_Name & "X", X, 1, 4);
      --     ARFF_Printing.Print_Data (Routine_Name & "Train_Data", Train_Data);
      X.Clear;
      Y.Clear;

      Save_State (Test_X, Test_Y, Train_X, Train_Y, Bunch);
   end if;

   if not Return_X_Y then
      Printing.Print_Strings ("Features", Bunch.Feature_Names);
   end if;
   Printing.Print_Value_Data_List ("Train features row 16", Train_X.Element (16));
   Printing.Print_Value_Data_List ("Test features row 16", Test_X.Element (16));
   New_Line;

   C_Init (aClassifier, Min_Split, Criterion.Gini_Criteria,
           Max_Leaf_Nodes => 6);

   --     --  Fit function adjusts weights according to data values so that
   --     --  better accuracy can be achieved
   Classification_Fit (aClassifier, Train_X, Train_Y, No_Weights);
   --     Printing.Print_Tree ("Diabetes Tree", aClassifier);
   --     Put_Line ("----------------------------------------------");
   New_Line;
   --
   --     for index in Train_Data.First_Index .. Train_Data.Last_Index loop
   --          if Base_Decision_Tree.Predict
   --            (aClassifier, Train_Data).Element (index).Element (1) =
   --                Labels.Element (index).Element (1) then
   --             Correct := Correct + 1;
   --          end if;
   --     end loop;
   --     Put_Line ("Prediction: " &
   --                 Float'Image (100.0 * Float (Correct) / Float (X_Data.Length)));
   --     New_Line;

   --
   --     Graphviz_Exporter.C_Init
   --       (Exporter, aClassifier.Attributes.Decision_Tree);
   --     Graphviz_Exporter.Export_Graphviz
   --       (Exporter, aClassifier.Attributes.Decision_Tree, Feature_Names => Features,
   --        Output_File_Name => To_Unbounded_String ("diabetes.dot"));

end Lesson_4;
