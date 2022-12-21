<?php

$id = $_GET['id'];
$filter = [ 'id' => $id ];
$options = [ 'projection' => ['_id' => 0] ];

$mng = new MongoDB\Driver\Manager("mongodb://localhost:27017");
$query = new MongoDB\Driver\Query($filter, $options);
$rows = $mng->executeQuery("nobel.laureates", $query);
$nobel = current($rows->toArray());
if (!empty($nobel)) {
    echo json_encode($nobel);
} else {
    echo "Invalid id provided\n";
}

?>
