# SwissAnki: Swiss German Learning Assistant

A Shiny R application that helps you learn Swiss German by creating Anki flashcards with AI-powered analysis, image generation, and audio pronunciation.

## 🌟 Features

- **AI-Powered Analysis**: Uses ChatGPT to analyze Swiss German words and phrases
- **Automatic Flashcard Creation**: Generates Anki cards with comprehensive grammar information
- **Image Generation**: Creates visual representations using DALL-E
- **Audio Pronunciation**: Generates audio files using OpenAI TTS
- **Grammar Templates**: Specialized templates for German grammar (nouns, verbs, adjectives, adverbs)
- **Cost Tracking**: Monitors API usage and costs
- **Interactive UI**: User-friendly interface with real-time feedback

## 🚀 Quick Start

### Prerequisites

- R (version 4.0 or higher)
- RStudio (recommended)
- Python 3.8+ with spaCy
- Anki with AnkiConnect add-on
- OpenAI API key

### Installation

1. **Clone the repository**
   ```bash
   git clone https://github.com/rasools/swissAnki.git
   cd swissAnki
   ```

2. **Install R dependencies**
   ```r
   # Install required R packages
   install.packages(c("shiny", "reticulate", "spacyr", "DT", "httr", "jsonlite", 
                     "base64enc", "magick", "shinythemes", "shinyjs"))
   ```

3. **Set up Python environment**
   ```bash
   # Create conda environment
   conda create -n swissAnki python=3.8
   conda activate swissAnki
   
   # Install spaCy and German language model
   pip install spacy
   python -m spacy download de_core_news_md
   ```

4. **Install AnkiConnect**
   - Open Anki
   - Go to Tools → Add-ons
   - Add code: `2055492159`
   - Restart Anki

5. **Set up OpenAI API**
   - Get an API key from [OpenAI](https://platform.openai.com/api-keys)
   - Create a file at `/Users/yourusername/Desktop/swissAnki_api.txt` with your API key

### Running the Application

1. **Start Anki** (required for AnkiConnect)

2. **Run the Shiny app**
   ```r
   # In R or RStudio
   setwd("path/to/swissAnki")
   shiny::runApp("app")
   ```

3. **Open your browser** and go to `http://localhost:3838`

## 📖 How to Use

### Basic Usage

1. **Enter Swiss German text** in the input field
2. **Click "Analysieren"** to get AI analysis
3. **Review the results** in the table
4. **Generate audio** for pronunciation
5. **Generate images** for visual learning
6. **Create Anki cards** with one click

### Grammar Templates

The app uses specialized templates for different parts of speech:

- **Nouns**: Gender, singular/plural forms, articles
- **Verbs**: All tenses, auxiliary verbs, separable prefixes
- **Adjectives**: Basic, comparative, superlative forms
- **Adverbs**: Basic, comparative, superlative forms

### Card Directions

- **Swiss German → English**: Front shows Swiss German, back shows English translation
- **English → Swiss German**: Front shows English, back shows Swiss German

## 🔧 Configuration

### API Settings

The app supports multiple OpenAI models:
- **GPT Models**: gpt-4o, gpt-4, gpt-3.5-turbo
- **Image Models**: dall-e-3, dall-e-2
- **TTS Models**: tts-1, tts-1-hd

### Cost Tracking

The app tracks usage and costs for:
- ChatGPT API calls
- DALL-E image generation
- TTS audio generation

## 📁 Project Structure

```
swissAnki/
├── app/
│   ├── app.R                 # Main Shiny application
│   └── temp_tables/          # Grammar templates
│       ├── adj.txt           # Adjective template
│       ├── adv.txt           # Adverb template
│       ├── entity.txt        # Entity template
│       ├── noun.txt          # Noun template
│       └── verb.txt          # Verb template
├── renv/                     # R environment files
├── swissAnki.Rproj          # RStudio project file
└── README.md                # This file
```

## 🛠️ Customization

### Adding New Grammar Templates

1. Create a new `.txt` file in `app/temp_tables/`
2. Follow the format: `Category | Details`
3. Add your custom fields
4. End with `End |`

### Modifying Existing Templates

Edit the template files in `app/temp_tables/` to customize:
- Field names
- Grammar categories
- Example sentence types

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## 📝 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- Built with [Shiny](https://shiny.rstudio.com/) for R
- Uses [spaCy](https://spacy.io/) for natural language processing
- Powered by [OpenAI](https://openai.com/) APIs
- Integrates with [Anki](https://apps.ankiweb.net/) for spaced repetition

## 📞 Support

If you encounter any issues or have questions:
1. Check the [Issues](https://github.com/rasools/swissAnki/issues) page
2. Create a new issue with detailed information
3. Include your R version, Python version, and error messages

---

**Happy learning Swiss German! 🇨🇭** 