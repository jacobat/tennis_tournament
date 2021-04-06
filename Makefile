app: elm.js style.css

elm.js: src/*.elm
	elm make --output elm.js src/Main.elm

style.css: src/*.elm
	npx tailwindcss build tailwind.css -o style.css

clean:
	rm elm.js style.css
