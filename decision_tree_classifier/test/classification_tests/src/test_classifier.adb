
with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Tests; use Classifier_Tests;

procedure Test_Classifier is

begin
   Put_Line ("Test Classifier started");
   Test_Classification_Toy;
--     Test_Probability;

end Test_Classifier;
