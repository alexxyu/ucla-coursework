===============================
* Explanation of Attack Alpha *
===============================

When trying different usernames that do not exist, I noticed that the profile page displays a
message like "USER does not exist". After inspecting the source code of the web server, I
noticed that the web server renders the username query parameter directly within the HTML
page template. (This is located in `code/router.js` for the `/profile` route)

Therefore, it is possible to inject code into the returned HTML page by sending it as the
username parameter in the GET request to the `/profile` endpoint. To accomplish this attack,
I can inject

    <script>var c = document.cookie; document.write(c)</script>

which is equivalent to

    <script>document.write(document.cookie)</script>

Then, the constructed URL is the following, once URL-encoded:

    http://localhost:3000/profile?username=%3Cscript%3Edocument.write(document.cookie)%3C/script%3E

===============================
* Explanation of Attack Bravo *
===============================

The constructed HTML page for this attack is very similar to that of Lab 2. The idea is to
create a form that mimics what is sent when performing a legimitate transfer through the
`/post_transfer` endpoint. This can be done through JavaScript by creating a form element,
adding the input elements for `destination_username` and `quantity`, and submitting the
form. The script does this upon page load, and after submitting the form, it redirects
the user to something innocuous, like `https://seedsecuritylabs.org/labs.html`.
