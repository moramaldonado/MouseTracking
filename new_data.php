<?
	$ID = "".rand( 0 , 999999999 )."-".time();
	$data = stripcslashes($_POST["data"]);
	$data = str_replace("\n", "\\n", $data);
	file_put_contents("data/$ID.json", $data);
	echo "$ID";
?>