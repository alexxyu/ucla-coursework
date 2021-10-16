<?php
  $mid = (int) $_POST['id'];
  $name = $_POST['name'];
  $rating = (int) $_POST['rating'];
  $comment = $_POST['comment'];

  if (!is_null($mid) && !is_null($name) && !is_null($rating) && !is_null($comment)) {
    $datetime = date("Y-m-d h:i:s");

    $db = new mysqli('localhost', 'cs143', '', 'class_db');
    if($db->connect_errno > 0) {
      die('Unable to connect to database [' . $db->connect_error . ']');
    }
    
    $statement = $db->prepare("INSERT INTO Review VALUES (?, ?, ?, ?, ?);");
    $statement->bind_param('ssiis', $name, $datetime, $mid, $rating, $comment);
    $statement->execute();
    $statement->close();
    $db->close();
  }
?>

<html>
  <body>
    <form action="review.php" method="post">
      <input type="hidden" name="id" value="<?php if (isset($_GET['id'])) { echo $_GET['id']; } else { echo $_POST['id']; } ?>">
      Name: <input type="text" name="name"><br>
      Rating: 
      <div>
        <input type="radio" name="rating" value="1">
        <label for="1">1 &nbsp;
        <input type="radio" name="rating" value="2">
        <label for="2">2 &nbsp;
        <input type="radio" name="rating" value="3">
        <label for="3">3 &nbsp;
        <input type="radio" name="rating" value="4">
        <label for="4">4 &nbsp;
        <input type="radio" name="rating" value="5">
        <label for="5">5 &nbsp;
      <div>
      Comment: <input type="text" name="comment"><br>
      <input type="submit">
    </form>
  </body>
</html>