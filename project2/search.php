<html>

<body>
  <?php if ($_GET['movie']) : ?>
    <?php

    $db = new mysqli('localhost', 'cs143', '', 'class_db');
    if ($db->connect_errno > 0) {
      die('Unable to connect to database [' . $db->connect_error . ']');
    }

    $words = explode(' ', $_GET['movie']);
    if (count($words) == 0) {
      die('Error: invalid request');
    }

    $statement_string = 'SELECT id, title FROM Movie WHERE ';
    $first = true;
    for ($i = 0; $i < count($words); $i++) {
      if (!$first) {
        $statement_string .= ' AND ';
      }
      $first = false;
      $statement_string .= 'LOWER(title) LIKE LOWER(?)';
      $words[$i] = '%' . $words[$i] . '%';
    }

    $statement = $db->prepare($statement_string);
    $statement->bind_param(str_repeat('s', count($words)), ...$words);
    $statement->execute();
    $statement->bind_result($mid, $title);

    echo '<h1>Movies for search: ' . $_GET['movie'] . '</h1>';
    echo '<ul>';
    while ($statement->fetch()) {
      echo '<li><a href="movie.php?id=' . $mid . '">' . $title . '</a></li>';
    }
    echo '</ul>';

    ?>
  <?php elseif ($_GET['actor']) : ?>
    <?php

    $db = new mysqli('localhost', 'cs143', '', 'class_db');
    if ($db->connect_errno > 0) {
      die('Unable to connect to database [' . $db->connect_error . ']');
    }

    $words = explode(' ', $_GET['actor']);
    if (count($words) == 0) {
      die('Error: invalid request');
    }

    $statement_string = 'SELECT id, first, last FROM Actor WHERE ';
    $first = true;
    for ($i = 0; $i < count($words); $i++) {
      if (!$first) {
        $statement_string .= ' AND ';
      }
      $first = false;
      $statement_string .= 'LOWER(CONCAT(first, \' \', last)) LIKE LOWER(?)';
      $words[$i] = '%' . $words[$i] . '%';
    }

    $statement = $db->prepare($statement_string);
    $statement->bind_param(str_repeat('s', count($words)), ...$words);
    $statement->execute();
    $statement->bind_result($aid, $first_name, $last_name);

    echo '<h1>Actors for search: ' . $_GET['actor'] . '</h1>';
    echo '<ul>';
    while ($statement->fetch()) {
      echo '<li><a href="actor.php?id=' . $aid . '">' . $first_name . ' ' . $last_name . '</a></li>';
    }
    echo '</ul>';

    ?>
  <?php else : ?>
    <form action="search.php" method="get">
      Search for a movie: <input type="text" name="movie"><br>
      Search for an actor: <input type="text" name="actor"><br>
      <input type="submit">
    </form>
  <?php endif; ?>
</body>

</html>
