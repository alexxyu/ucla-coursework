<?php
  if(!isset($_POST['mid']) && (!isset($_GET['id']) || empty($_GET['id']))) {
    die('Error: invalid request');
  }

  $mid = (int) $_POST['mid'];
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

    header("Location: movie.php?id=" . $mid);
    die();
  }
?>

<html>
  <body>
    <form action="review.php" method="post">
      <input type="hidden" name="mid" value="<?php echo $_GET['id']; ?>">
      Name: <input type="text" name="name" required><br>
      Rating: 
      <div>
        <input type="radio" name="rating" value="1" required>
        <label for="1">1 &nbsp;
        <input type="radio" name="rating" value="2" required>
        <label for="2">2 &nbsp;
        <input type="radio" name="rating" value="3" required>
        <label for="3">3 &nbsp;
        <input type="radio" name="rating" value="4" required>
        <label for="4">4 &nbsp;
        <input type="radio" name="rating" value="5" required>
        <label for="5">5 &nbsp;
      <div>
      Comment: <input type="text" name="comment" required><br>
      <input type="submit">
    </form>
  </body>
</html>