# Manipulating the DOM

---

## Querying Elements

You can find out all sorts of information about an element:

```javascript
let container = document.getElementById("container");

container.clientHeight; // the height of the element (in pixels)
container.clientWidth; // the width of the element (in pixels)
container.style.borderColor; // the inline style border colour
window.getComputedStyle(container).borderColor; // the border colour - including CSS
container.innerHTML; // the html inside the element (e.g. "<p>Text</p>")
container.textContent; // the text inside the element (e.g. "Text")
container.getBoundingClientRect(); // returns the position of the element relative to the page
```

You can store these in variables to use later:

```javascript
let height = container.clientHeight;
container.style.height = (height + 300) + "px"; // add 300 pixels to the height of the container
```

**Note**: it's always better to not query the DOM if you can avoid it. If you can use variables to keep track of changes your code will be much easier to understand and it will run faster.

---

## Document Properties

You can find useful information out about the page:

```javascript
window.pageYOffset; // the current vertical scroll position
window.pageXOffset; // the current horizontal scroll position

window.innerHeight; // the height of the viewport
window.innerWidth; // the width of the viewport

document.body.clientHeight; // the height of the document
document.body.clientWidth; // the width of the document
```

**Note**: the [`window` object](https://developer.mozilla.org/en-US/docs/Web/API/Window) is part of the **BOM** (Browser Object Model). As well as having the window specific properties/methods, it actually represents the **global scope** in a browser.

---

## Manipulating Elements

You can also edit the styling of the element directly by setting sub-properties of the `style` property:

```javascript
let container = document.getElementById("container");

container.style.border = "1px solid red";
container.style.marginTop = "20px"; // notice we have to write marginTop, not margin-top
container.style.position = "absolute";
container.style.left = "20px";
container.style.top = "20px";
```

You can set the height and width too:

```javascript
let container = document.getElementById("container");

container.style.height = "200px";
container.style.width = "200px";
```

It is preferable to use CSS classes where you can, but sometimes you will need to use the `.style` property if the styling is dependent on values calculated by JavaScript.

---

## Manipulating Elements

You can also change the text inside an element:

```javascript
let title = document.getElementById("title");
title.textContent = "New Title"; // replaces text of #title
```

**Note**: be careful, setting `.textContent` will remove everything inside the element

---

## Attributes

We can query/edit any attribute of an element:

```html
<input id="age" name="age" value="20" />
```

```javascript
let input = document.getElementById("age");

input.getAttribute("name"); // returns "age"
input.getAttribute("value"); // returns "20" (as a string)
input.setAttribute("value", "600"); // set the default value attribute to "600"
input.removeAttribute("disabled"); // remove the disabled attribute
```

---

## Form Fields

To get the *actual* value of the input (as opposed to the default value), we can use the `.value` property:

```javascript
input.value; // a string containing the current value of the input
```

**Note**: The `.value` property will *always* return a string - so be careful if you're doing any addition

We can also call the `.focus()` method on a form field to give it focus:

```javascript
input.focus(); // gives the input focus
```

---

## Data Attributes

Sometimes we want to let JavaScript know something about an element that isn't a standard property.

We can use `data-*` attributes for this.

For example, if we wanted to store additional information about some books:

```html
<ul id="books">
    <li data-id="12" data-author="Marijn Haverbeke">Eloquent JavaScript</li>
    <li data-id="35" data-author="Douglas Crockford">JavaScript: The Good Parts</li>
    <li data-id="59" data-author="David Flanagan">JavaScript: The Definitive Guide</li>
</ul>
```

```javascript
let first = document.getElementById("books").firstElementChild;

if (first) {
    console.log(first.dataset.id);
    console.log(first.dataset.author);
}
```

**Note**: you should only use `data-*` attributes as a last resort, as reading from the DOM is slow. Ideally the data would be used and stored only in JavaScript.

---

## Additional Reading

- [MDN: `window.getComputedStyle()`](https://developer.mozilla.org/en-US/docs/Web/API/Window/getComputedStyle)
- [MDN: `getBoundingClientRect()`](https://developer.mozilla.org/en-US/docs/Web/API/Element/getBoundingClientRect)
- [MDN: `data-*` Attributes](https://developer.mozilla.org/en-US/docs/Learn/HTML/Howto/Use_data_attributes)
- [MDN: `window`](https://developer.mozilla.org/en-US/docs/Web/API/Window)
