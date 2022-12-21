<?php
// get the id parameter from the request
$id = intval($_GET['id']);

// set the Content-Type header to JSON, 
// so that the client knows that we are returning JSON data
header('Content-Type: application/json');

$db = new mysqli('localhost', 'cs143', '', 'class_db');

$statement = $db->prepare("SELECT given_name, family_name, gender, birth_date, birth_city, birth_country FROM Person WHERE id = ?");
$statement->bind_param('i', $id);
$statement->execute();
$statement->bind_result($given_name, $family_name, $gender, $birth_date, $birth_city, $birth_country);
$is_person = $statement->fetch();
$statement->close();

$output = (object) [
    "id" => strval($id)
];

if ($is_person) {
    if ($given_name) {
        $output->givenName = (object) ["en" => $given_name];
    }
    if ($family_name) {
        $output->familyName = (object) ["en" => $family_name];
    }
    if ($gender) {
        $output->gender = $gender;
    }
    if ($birth_date || $birth_country || $birth_city) {
        $birth_object = (object) [];
        if ($birth_date) {
            $birth_object->date = $birth_date;
        }
        if ($birth_country || $birth_city) {
            $birth_place_object = (object) [];
            if ($birth_city) {
                $birth_place_object->city = (object) ["en" => $birth_city];
            }
            if ($birth_country) {
                $birth_place_object->country = (object) ["en" => $birth_country];
            }
            $birth_object->place = $birth_place_object;
        }
        $output->birth = $birth_object;
    }
} else {
    $statement = $db->prepare("SELECT org_name, founded_date, founded_city, founded_country FROM Organization WHERE id = ?");
    $statement->bind_param('i', $id);
    $statement->execute();
    $statement->bind_result($org_name, $founded_date, $founded_city, $founded_country);
    $statement->fetch();
    $statement->close();

    if ($org_name) {
        $output->orgName = (object) ["en" => $org_name];
    }
    if ($founded_date || $founded_city || $founded_country) {
        $founded_object = (object) [];
        if ($founded_date) {
            $founded_object->date = $founded_date;
        }
        if ($founded_city ||  $founded_country) {
            $founded_place_object = (object) [];
            if ($founded_city) {
                $founded_place_object->city = (object) ["en" => $founded_city];
            }
            if ($founded_country) {
                $founded_place_object->country = (object) ["en" => $founded_country];
            }
            $founded_object->place = $founded_place_object;
        }
        $output->founded = $founded_object;
    }
}

$prizes = array();
$prize_ids = array();

$statement = $db->prepare("SELECT prize_id, year, category, sort_order FROM Prize WHERE winner_id = ?");
$statement->bind_param('i', $id);
$statement->execute();
$statement->bind_result($prize_id, $year, $category, $sort_order);
while ($statement->fetch()) {
    $prize_object = (object) [];
    if ($year) {
        $prize_object->awardYear = strval($year);
    }
    if ($category) {
        $prize_object->category = (object) ["en" => $category];
    }
    if ($sort_order) {
        $prize_object->sortOrder = strval($sort_order);
    }

    array_push($prize_ids, $prize_id);
    array_push($prizes, $prize_object);
}
$statement->close();

$i = 0;
foreach ($prizes as $prize) {
    $affiliations = array();

    $statement = $db->prepare("SELECT affiliation_name, city, country FROM Affiliation WHERE prize_id = ?");
    $statement->bind_param('i', intval($prize_id));
    $statement->execute();
    $statement->bind_result($name, $city, $country);
    while ($statement->fetch()) {
        $affiliation_object = (object) [];
        if ($name) {
            $affiliation_object->name = (object) ["en" => $name];
        }
        if ($city) {
            $affiliation_object->city = (object) ["en" => $city];
        }
        if ($country) {
            $affiliation_object->country = (object) ["en" => $country];
        }
        array_push($affiliations, $affiliation_object);
    }
    $statement->close();

    if (count($affiliations) != 0) {
        $prizes[$i]->affiliations = $affiliations;
    }
    $i++;
}

$output->nobelPrizes = $prizes;

echo json_encode($output);
